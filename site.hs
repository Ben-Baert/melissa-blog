--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Data.List (isSuffixOf, intersperse, intercalate)
import           Text.Blaze.Html(toHtml)
import           System.FilePath.Posix ((</>), takeBaseName, takeDirectory, splitFileName)
import           Data.Char(toUpper)
--------------------------------------------------------------------------------
replDash :: String -> String
replDash = map (\x -> if x == '-' then ' ' else x)

title :: String -> String
title = unwords .  map firstUpper  . words
    where firstUpper (x:xs) = toUpper x : xs

prettyCategory = title . replDash

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


--------------------------------------------------------------------------------
cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute
  where
      createIndexRoute ident = takeDirectory p </> takeBaseName p </> "index.html"
                                  where p = toFilePath ident

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanIndex)

cleanIndexHtmls :: Item String -> Compiler (Item String)
cleanIndexHtmls = return . fmap (replaceAll pattern replacement)
    where
          pattern = "/index.html"
          replacement = const "/"

cleanIndex :: String -> String
cleanIndex url
    | idx `isSuffixOf` url = take (length url - length idx) url
    | otherwise            = url
        where idx = "index.html"
                       

--------------------------------------------------------------------------------
teaserCtx :: Context String
teaserCtx = 
        teaserField "teaser" "content" `mappend`
        defaultContext

categoryField' :: String -> Tags -> Context a
categoryField' = 
            tagsFieldWith getCategory render (mconcat . intersperse ", ")
                where 
                    render tag _ = Just $ toHtml $ prettyCategory tag
                    getCategory = return . return . takeBaseName . takeDirectory . toFilePath

categoryCtx :: Tags -> Tags -> Context String
categoryCtx tags category =
        categoryField' "category" category `mappend`
        tagsField     "tags"     tags     `mappend`
        teaserCtx

--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration 
            { deployCommand = "# Temporarily store uncommited changes\n\
                               \git stash\n\
                               \\n\
                               \# Verify correct branch\n\
                               \git checkout develop\n\
                               \\n\
                               \# Build new files\n\
                               \stack exec melissa-blog clean\n\
                               \stack exec melissa-blog build\n\
                               \\n\
                               \# Get previous files\n\
                               \git fetch --all\n\
                               \git checkout -b master --track origin/master\n\
                               \\n\
                               \# Overwrite existing files with new files\n\
                               \rsync -a --exclude-from=.gitignore  --delete _site/ .\n\
                               \\n\
                               \# Commit\n\
                               \git add -A\n\
                               \git commit -m 'Publish.'\n\
                               \\n\
                               \# Push\n\
                               \git push origin master:master\n\
                               \\n\
                               \# Restoration\n\
                               \git checkout develop\n\
                               \git branch -D master\n\
                               \git stash pop\n"
         }
