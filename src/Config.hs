{-# LANGUAGE OverloadedStrings #-}
module Config where

import Hakyll 

myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = "Melissa's blog"
    , feedDescription = "This feed provides all content from all categories from Melissa's blog"
    , feedAuthorName  = "Melissa Jade Katon"
    , feedAuthorEmail = "hello@melissakaton.com"
    , feedRoot        = "http://www.melissakaton.com"
    }


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
