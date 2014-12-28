{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend, mconcat)
import           Hakyll


main :: IO ()
main = hakyll $ do
    match ("images/**" .|. "posts/re2c/images/*") $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match ("posts/re2c/*" .||. "posts/london/*") $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "posts/re2c/codez/**" $ do
        route $ customRoute $ (++ ".txt") . toFilePath
        compile copyFileCompiler

    create ["london.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/london/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Лондон"              `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/post-list.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

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
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            let indexCtx =
                    constField "title" "home" `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

    create ["feed/atom.xml"] $ do
        route idRoute
        compile $ do
            loadAll "posts/*"
                >>= recentFirst
                >>= renderAtom (feedConfiguration "all posts") feedCtx

    create ["feed/rss.xml"] $ do
        route idRoute
        compile $ do
            loadAll "posts/*"
                >>= recentFirst
                >>= renderRss (feedConfiguration "all posts") feedCtx

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
