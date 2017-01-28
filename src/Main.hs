{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where
import           Hakyll     hiding      (match)
--------------------------------------------------------------------------------

import           CleanRoutes
import           Config
import           Context
import           Match
import           PrettyCategory
import           RelatedPosts
import           Utils
--------------------------------------------------------------------------------

main :: IO ()
main = hakyllWith config $ do
    match ("pictures/*.jpg" .||. "pictures/*.png" .||. "pictures/*.gif") $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.rst", "contact.md"]) $ do
        route   $ cleanRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultCtx
            >>= relativizeUrls
            >>= cleanIndexUrls

    authors <- buildTagsWith getAuthors postPattern (fromCapture "author/*.html")

    tagsRules authors $ \author pattern -> do
        let title = "Posts written by " ++ author
        route cleanRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx =
                    constField "title" title `mappend`
                    listField "posts" teaserCtx (return posts) `mappend`
                    defaultCtx
            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
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
                    defaultCtx

            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls
                >>= cleanIndexUrls

    tagsRules categories $ \category pattern -> do
        let title = "Posts in " ++ (prettyCategory category)
        route $ gsubRoute "categories/" (const "") `composeRoutes`
                cleanRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx =
                    constField "title" title                        `mappend`
                    listField "posts" teaserCtx (return posts) `mappend`
                    defaultCtx

            makeItem ""
                >>= loadAndApplyTemplate "templates/category.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls
                >>= cleanIndexUrls

{-
    albums <- buildCategories "pictures/**" (fromCapture "album/*.html")

    tagsRules albums $ \category pattern -> do
        let title = "Pictures in " ++ (prettyCategory category)
        route cleanRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx =
                    constField "title" title                        `mappend`
                    listField "posts" defaultCtx (return posts) `mappend`
                    defaultCtx

            makeItem ""
                >>= loadAndApplyTemplate "templates/album.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls
                >>= cleanIndexUrls
-}
    trips <- buildCategories "travel/*/day-*.md" (fromCapture "travel/*.html")

{-
    tagsRules trips $ \trip pattern -> do
        let title = "Trip to " ++ (prettyCategory trip)
        let metaIdent = fromFilePath $ "travel/" ++ trip ++ "/metadata.yml"
        departureDate <- getMetadataField' metaIdent "departure-date"
        route cleanRoute
        compile $ do
            posts <- chronological =<< loadAll pattern
            let ctx =
                    constField "title" title `mappend`
                    constField "departureDate" departureDate `mappend`
                    listField "posts" teaserCtx (return posts) `mappend`
                    defaultCtx

            makeItem ""
                >>= loadAndApplyTemplate "templates/travel-overview.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls
                >>= cleanIndexUrls

-}
    match "travel/*/meta.md" $ do
        compile pandocCompiler

    match "blog/**" $ do
        route $ gsubRoute "posts/" (const "") `composeRoutes` 
                cleanRoute

        compile $ do
            let ctx = 
                    previousPostField "previousPost" `mappend`
                    nextPostField "nextPost" `mappend`
                    categoryCtx tags categories
            pandocCompiler
                >>= saveSnapshot "content"
                >>= loadAndApplyTemplate "templates/post.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx 
                >>= relativizeUrls
                >>= cleanIndexUrls

{-

    match "pictures/**" $ do
        route cleanRoute 
                
        
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/picture-post.html" defaultCtx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" defaultCtx
            >>= relativizeUrls
            >>= cleanIndexUrls
 -}   
    match "travel/*/day-*.md" $ do
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
                    defaultCtx

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls
                >>= cleanIndexUrls

{-
    create ["pictures.html"] $ do
        route cleanRoute 
        compile $ do
            let pictureCtx = 
                    field "albums" (\_ -> renderTagList albums)            `mappend`   
                    constField "title" "Albums"                          `mappend`
                    defaultCtx

            makeItem ""
                >>= loadAndApplyTemplate "templates/pictures.html" pictureCtx
                >>= loadAndApplyTemplate "templates/default.html" pictureCtx
                >>= relativizeUrls
                >>= cleanIndexUrls

-}
    create ["travel.html"] $ do
        route cleanRoute
        compile $ do
            tripOverviews <- recentFirst =<< loadAll "travel/*/meta.md" 
            let travelCtx = 
                    listField "trips" defaultCtx (return tripOverviews) `mappend`
                    constField "title" "Trips"                `mappend`
                    defaultCtx

            makeItem ""
                >>= loadAndApplyTemplate "templates/trips.html" travelCtx
                >>= loadAndApplyTemplate "templates/default.html" travelCtx
                >>= relativizeUrls
                >>= cleanIndexUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- (fmap (take 10)) . recentFirst =<< loadAll postPattern
            let indexCtx =
                    listField "posts" (categoryCtx tags categories) (return posts) `mappend`
                    defaultCtx

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls
                >>= cleanIndexUrls
    
    match "404.html" $ do
        route idRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultCtx

    match "templates/*" $ compile templateCompiler

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = defaultCtx `mappend` bodyField "description"
            posts <- fmap (take 10) . recentFirst =<<
                    loadAllSnapshots postPattern "content"
            renderAtom myFeedConfiguration feedCtx posts


