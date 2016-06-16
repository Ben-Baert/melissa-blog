{-# LANGUAGE OverloadedStrings #-}
module Context 
    ( teaserCtx,
      categoryCtx,
      defaultCtx
    ) where

import           Hakyll
import           PrettyCategory
import           Breadcrumbs                     (breadCrumbField)
import           Data.Monoid                     (mappend, mconcat)
import           Text.Blaze.Html.Renderer.String (renderHtml)
import           Data.List                       (intersperse)
import           System.FilePath.Posix           (takeBaseName, takeDirectory)
import           Text.Blaze.Html                 (toHtml)
import           Data.Maybe                      (fromMaybe)
import qualified Data.Map                        as M


authorField :: Context a 
authorField = field "author" $ \item -> do 
    metadata <- getMetadata (itemIdentifier item)
    return $ fromMaybe "Melissa Katon" $  M.lookup "authors" metadata

teaserCtx :: Context String
teaserCtx = 
        teaserField "teaser" "content" `mappend`
        defaultCtx

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

defaultCtx :: Context String
defaultCtx = 
            authorField                   `mappend`
            breadCrumbField "breadcrumbs" `mappend`
            defaultContext
