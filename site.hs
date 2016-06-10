--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll


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
        route   $ setExtension ""
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension ""
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive"] $ do
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
                    constField "title" "Home"                `mappend`
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
                               \git fetch -all\n\
                               \git checkout -b master --track origin/master\n\
                               \\n\
                               \# Overwrite existing files with new files\n\
                               \rsync -a --filter='P _site/' --delete-excluded _site/ .\n\
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
