{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where
import           Data.Maybe (fromMaybe)
import           Hakyll hiding (match)
--------------------------------------------------------------------------------

import           CleanRoutes
import           Config
import           Context
import           Match
import           PrettyCategory
--------------------------------------------------------------------------------

main :: IO ()
main = hakyllWith config $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.rst", "contact.md"]) $ do
        route   $ cleanRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls
            >>= cleanIndexUrls

    tags <- buildTags "blog/**.md" (fromCapture "tags/*.html")
    categories <- buildCategories "blog/**.md" (fromCapture "categories/*.html")

    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged " ++ (prettyCategory tag)
        route cleanRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = 
                    constField "title" title                        `mappend`
                    listField "posts" teaserCtx (return posts) `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls
                >>= cleanIndexUrls

    tagsRules categories $ \category pattern -> do
        let title = "Posts in " ++ (prettyCategory category)
        route cleanRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx =
                    constField "title" title                        `mappend`
                    listField "posts" teaserCtx (return posts) `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/category.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls
                >>= cleanIndexUrls

    albums <- buildCategories "pictures/**.md" (fromCapture "album/*.html")

    tagsRules albums $ \category pattern -> do
        let title = "Pictures in " ++ (prettyCategory category)
        route cleanRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx =
                    constField "title" title                        `mappend`
                    listField "posts" defaultContext (return posts) `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/album.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls
                >>= cleanIndexUrls

    trips <- buildCategories "travel/**.md" (fromCapture "travel/*.html")

    tagsRules trips $ \trip pattern -> do
        let title = "Trip to " ++ (prettyCategory trip)
        route cleanRoute
        compile $ do
            posts <- chronological =<< loadAll pattern
            -- firstDay <- head posts
            let ctx =
                    constField "title" title `mappend`
                    listField "posts" teaserCtx (return posts) `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/trip.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls
                >>= cleanIndexUrls

    match "blog/**" $ do
        route $ gsubRoute "posts/" (const "") `composeRoutes` 
                cleanRoute

        compile $ pandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html"    (categoryCtx tags categories) 
            >>= loadAndApplyTemplate "templates/default.html" (categoryCtx tags categories) 
            >>= relativizeUrls
            >>= cleanIndexUrls

    match "pictures/**" $ do
        route $ cleanRoute 
                
        
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/picture-post.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls
            >>= cleanIndexUrls
    
    match "travel/**" $ do
        route cleanRoute

        compile $ pandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/trip-report.html" (categoryCtx tags trips)
            >>= loadAndApplyTemplate "templates/default.html" (categoryCtx tags trips)
            >>= relativizeUrls
            >>= cleanIndexUrls

    create ["blog.html"] $ do
        route cleanRoute
        compile $ do
            posts <- (fmap (take 10)) . recentFirst =<< loadAll "blog/**"
            let archiveCtx =
                    field "categories" (\_ -> renderTagList categories) `mappend`
                    listField "posts" teaserCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls
                >>= cleanIndexUrls

    create ["pictures.html"] $ do
        route cleanRoute 
        compile $ do
            let pictureCtx = 
                    field "albums" (\_ -> renderTagList albums)            `mappend`   
                    constField "title" "Albums"                          `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/pictures.html" pictureCtx
                >>= loadAndApplyTemplate "templates/default.html" pictureCtx
                >>= relativizeUrls
                >>= cleanIndexUrls

    create ["travel.html"] $ do
        route cleanRoute
        compile $ do
            let travelCtx = 
                    field "trips" (\_ -> renderTagList trips) `mappend`
                    constField "title" "Trips"                `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/trips.html" travelCtx
                >>= loadAndApplyTemplate "templates/default.html" travelCtx
                >>= relativizeUrls
                >>= cleanIndexUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- (fmap (take 10)) . recentFirst =<< loadAll "*/**.md"
            let indexCtx =
                    listField "posts" (categoryCtx tags categories) (return posts) `mappend`
                    constField "title" "Home"                                      `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls
                >>= cleanIndexUrls

    match "templates/*" $ compile templateCompiler


