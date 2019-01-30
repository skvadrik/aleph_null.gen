{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mappend, mconcat)
import Hakyll
import Control.Monad (forM_)

main :: IO ()
main = hakyll $ do
    match ("images/**" .||. "posts/re2c/images/**") $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match ("posts/life/*") $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" postCtx
            >>= loadAndApplyTemplate "templates/art.html"  postCtx
            >>= relativizeUrls

    match ("posts/re2c/*" .||. "posts/util/*") $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" postCtx
            >>= loadAndApplyTemplate "templates/code.html" postCtx
            >>= relativizeUrls

    match ("posts/re2c/codez/**" .||. "posts/util/codez/**") $ do
        route $ customRoute $ (++ ".txt") . toFilePath
        compile copyFileCompiler

    create ["re2c.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/re2c/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "re2c"                `mappend`
                    defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/post-list.html" archiveCtx
                >>= loadAndApplyTemplate "templates/code.html" archiveCtx
                >>= relativizeUrls

    create ["util.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/util/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "util"                `mappend`
                    defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/post-list.html" archiveCtx
                >>= loadAndApplyTemplate "templates/code.html" archiveCtx
                >>= relativizeUrls

    create ["life.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/life/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "life"                `mappend`
                    defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/post-list.html" archiveCtx
                >>= loadAndApplyTemplate "templates/art.html" archiveCtx
                >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            let indexCtx =
                    constField "title" "home" `mappend`
                    defaultContext
            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/art.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

    forM_ [ ("feed/atom.xml", renderAtom)
          , ("feed/rss.xml", renderRss)
          ] $ \(path, render) -> do
        create [path] $ do
            route idRoute
            compile $ do
                loadAll ("posts/life/*"
                    .||. "posts/re2c/*"
                    .||. "posts/util/*")
                    >>= recentFirst
                    >>= render (feedConfiguration "all posts") feedCtx

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

feedCtx :: Context String
feedCtx = mconcat
    [ bodyField "description"
    , defaultContext
    ]

feedConfiguration :: String -> FeedConfiguration
feedConfiguration title = FeedConfiguration
    { feedTitle = "aleph null - " ++ title
    , feedDescription = "skvadrik's blog"
    , feedAuthorName = "Ulya Trofimovich"
    , feedAuthorEmail = "skvadrik@gmail.com"
    , feedRoot = "http://skvadrik.github.io/aleph_null"
    }
