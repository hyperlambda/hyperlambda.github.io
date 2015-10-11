{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import           Data.Monoid
import           Hakyll
import           Text.Pandoc.Options
import Control.Monad
import Data.List
import Data.Maybe
import Control.Applicative ((<$>))
import System.FilePath
import           Data.Binary                   (Binary (..))
import           Data.Typeable                 (Typeable)
import System.Process (rawSystem)
import System.Exit
import System.IO (hPutStrLn, stderr)
import Text.XML.HXT.Core as HXT
import GHC.IO.Encoding as E

--------------------------------------------------------------------------------
main :: IO ()
main = do
  setLocaleEncoding E.utf8
  setFileSystemEncoding E.utf8
  setForeignEncoding E.utf8
  hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/main.scss" $ do
        route $ setExtension "css"
        compile sassCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls
            >>= slashUrlsCompiler

    match "posts/*" $ do
        route   blogRoute

        compile $ do
            ident <- getUnderlying
            toc <- getMetadataField ident "toc"
            let writerSettings = case toc of
                                    (Just "yes")  -> myWriterOptionsToc
                                    (Just "no")  -> myWriterOptions
                                    Nothing     -> myWriterOptions
            pandocCompilerWith myReaderOptions writerSettings
                >>= saveSnapshot "content"
                >>= loadAndApplyTemplate "templates/post.html"    postCtx
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= slashUrlsCompiler
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
                >>= slashUrlsCompiler


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" (myTeaserCtx <> postCtx) (return $ take 7 posts) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= slashUrlsCompiler
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


    create ["rss.xml"] $ do
         route idRoute
         compile $ do
             posts <- fmap (take 10) . recentFirst =<<
                 loadAllSnapshots "posts/*" "content"
             renderRss feedConfiguration feedContext posts
             >>= slashUrlsCompiler

--------------------------------------------------------------------------------

myTeaserCtx :: Context String
myTeaserCtx = field "teaser" teaserBody
-- myTeaserCtx = teaserField "teaser" "content" <> field "content" (\item -> itemBody <$> loadSnapshot (itemIdentifier item) "content")


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

feedContext :: Context String
feedContext = mconcat
     [ rssBodyField "description"
     , rssTitleField "title"
     , wpUrlField "url"
     , dateField "date" "%B %e, %Y"
     ]


feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle = "Hyper Lambda"
    , feedDescription = "All things functional"
    , feedAuthorName = "Sarunas Valaskevicius"
    , feedAuthorEmail = "rakatan@gmail.com"
    , feedRoot = "http://www.hyperlambda.com"
    }

rssBodyField :: String -> Context String
rssBodyField key = field key (\item -> do
                                teaser <- teaserBody item
                                return $ withUrls wordpress . withUrls absolute $ teaser)
  where
    wordpress = replaceAll "/index.html" (const "/")
    absolute x
      | head x == '/' = feedRoot feedConfiguration ++ x
      | take 8 x == "/files/" = feedRoot feedConfiguration ++ drop 1 x
      | otherwise = x

empty :: Compiler String
empty = return ""

rssTitleField :: String -> Context a
rssTitleField key = field key $ \i -> do
    value <- getMetadataField (itemIdentifier i) "title"
    let value' = liftM (replaceAll "&" (const "&amp;")) value
    maybe empty return value'


toWordPressUrl :: FilePath -> String
toWordPressUrl url =
    replaceAll "/index.html" (const "/") (toUrl url)

wpUrlField :: String -> Context a
wpUrlField key = field key $
    fmap (maybe "" toWordPressUrl) . getRoute . itemIdentifier    

teaserBody :: Item String -> Compiler String
teaserBody item = do
    body <- itemBody <$> loadSnapshot (itemIdentifier item) "content"
    return $ extractTeaser . maxLengthTeaser . compactTeaser $ body
  where
    extractTeaser :: String -> String
    extractTeaser [] = []
    extractTeaser xs@(x : xr)
        | "<!-- more -->" `isPrefixOf` xs = []
        | otherwise = x : extractTeaser xr

    maxLengthTeaser :: String -> String
    maxLengthTeaser s = if isNothing $ findIndex (isPrefixOf "<!-- more -->") (tails s)
                            then unwords (take 60 (words s))
                            else s

    compactTeaser :: String -> String
    compactTeaser =
        replaceAll "<iframe [^>]*>" (const "") .
        replaceAll "<img [^>]*>" (const "") .
        replaceAll "<p>" (const "") .
        replaceAll "</p>" (const "") .
        replaceAll "<blockquote>" (const "") .
        replaceAll "</blockquote>" (const "") .
        replaceAll "<strong>" (const "") .
        replaceAll "</strong>" (const "") .
        replaceAll "<ol>" (const "") .
        replaceAll "</ol>" (const "") .
        replaceAll "<ul>" (const "") .
        replaceAll "</ul>" (const "") .
        replaceAll "<li>" (const "") .
        replaceAll "</li>" (const "") .
        replaceAll "<h[0-9][^>]*>" (const "") .
        replaceAll "</h[0-9]>" (const "") .
        replaceAll "<pre.*" (const "") .
        replaceAll "<a [^>]*>" (const "") .
        replaceAll "</a>" (const "") .
        replaceAll "<div [^>]*>" (const "") .
        replaceAll "</div>" (const "") .
        removeToc

    removeToc :: String -> String
    removeToc s = concat $ (runLA . xshow) (hread >>> manipulate) $ "<html><body>"++s++"</body></html>"
      where
        manipulate = processTopDown (
          none
          `HXT.when`
          (isElem >>> hasName "div" >>> getAttrValue "id" >>> isA (=="toc"))
          )

slashUrlsCompiler :: Item String -> Compiler (Item String)
slashUrlsCompiler item = do
    myRoute <- getRoute $ itemIdentifier item
    return $ case myRoute of
        Nothing -> item
        Just _ -> fmap slashUrls item

slashUrls :: String -> String 
slashUrls = fileLinks . withUrls convert
  where
    convert = replaceAll "/index.html" (const "/")
    fileLinks = replaceAll "/files/" (const "/files/")


blogRoute :: Routes
blogRoute  =
     cleanDate 
     `composeRoutes` gsubRoute ".html" (const "/index.html")
     `composeRoutes` gsubRoute ".md" (const "/index.html")
     `composeRoutes` gsubRoute ".lhs" (const "/index.html")

cleanDate :: Routes
cleanDate = customRoute removeDatePrefix

removeDatePrefix :: Identifier -> FilePath
removeDatePrefix ident = replaceFileName file (drop 11 $ takeFileName file)
  where file = toFilePath ident

--------------------------------------------------------------------------------
newtype SassRunner = SassRunner FilePath
    deriving (Binary, Eq, Ord, Show, Typeable)

instance Writable SassRunner where
    write dst (Item _ (SassRunner src)) = do
      code <- rawSystem "bundle" ["exec", "sass", "--trace", "-t", "compressed", src, dst]
      case code of
        ExitSuccess -> return ()
        ExitFailure e -> hPutStrLn stderr $ "Could not run sass for "++src++" to "++dst++": the command has returned error code "++show e

sassCompiler :: Compiler (Item SassRunner)
sassCompiler = do
    path <- getResourceFilePath
    debugCompiler $ "Compiling with sass: " ++ path
    makeItem $ SassRunner path


