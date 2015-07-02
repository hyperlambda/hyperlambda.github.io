--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Text.Pandoc.Options


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ do
                ident <- getUnderlying
                toc <- getMetadataField ident "toc"
                let writerSettings = case toc of
                                        (Just "yes")  -> myWriterOptionsToc
                                        Nothing     -> myWriterOptions
                pandocCompilerWith myReaderOptions writerSettings
                >>= loadAndApplyTemplate "templates/post.html"    postCtx
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

myWriterOptions :: WriterOptions
myWriterOptions = defaultHakyllWriterOptions {
      writerReferenceLinks = True
    , writerHtml5 = True
    , writerHighlight = True
    }

myWriterOptionsToc :: WriterOptions
myWriterOptionsToc = myWriterOptions {
      writerTableOfContents = True
    , writerTOCDepth = 2
    , writerTemplate = "$if(toc)$<div id=\"toc\">$toc$</div>$endif$\n$body$"
    , writerStandalone = True
    }

myReaderOptions :: ReaderOptions
myReaderOptions = defaultHakyllReaderOptions
