--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Data.List (isSuffixOf)
import           System.FilePath.Posix ((</>), takeBaseName, takeDirectory, splitFileName)

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

    tags <- buildTags "posts/**.md" (fromCapture "tags/*.html")
    categories <- buildCategories "posts/**.md" (fromCapture "categories/*.html")

    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged " ++ tag
        route cleanRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = 
                    constField "title" title                        `mappend`
                    listField "posts" defaultContext (return posts) `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls
                >>= cleanIndexUrls

    tagsRules categories $ \category pattern -> do
        let title = "Posts in " ++ category
        route cleanRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx =
                    constField "title" title                        `mappend`
                    listField "posts" defaultContext (return posts) `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/category.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls
                >>= cleanIndexUrls

    match "posts/**" $ do
        route $ gsubRoute "posts/" (const "") `composeRoutes` 
                cleanRoute

        compile $ pandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html"    defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls
            >>= cleanIndexUrls

    match "picture-posts/*" $ do
        route $ gsubRoute "picture-posts/" (const "pictures/") 
                `composeRoutes` cleanRoute
        
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/picture-post.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls
            >>= cleanIndexUrls

    create ["archive.html"] $ do
        route cleanRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/**"
            let archiveCtx =
                    listField "posts" defaultContext (return posts) `mappend`
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
            pictureposts <- loadAll "picture-posts/*"
            let pictureCtx = 
                    listField "posts" defaultContext (return pictureposts) `mappend`
                    constField "title" "Pictures"                          `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/pictures.html" pictureCtx
                >>= loadAndApplyTemplate "templates/default.html" pictureCtx
                >>= relativizeUrls
                >>= cleanIndexUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/**"
            let indexCtx =
                    listField "posts" teaserCtx (return posts) `mappend`
                    constField "title" "Home"                  `mappend`
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
teaserCtx = teaserField "teaser" "content" `mappend`
            defaultContext

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
