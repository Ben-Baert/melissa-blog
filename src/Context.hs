{-# LANGUAGE OverloadedStrings #-}
module Context 
    ( teaserCtx,
      categoryCtx,
      defaultCtx
    ) where

import           Hakyll (Context, teaserField, Tags, tagsFieldWith, toFilePath, tagsField, defaultContext)
import           PrettyCategory
import           Breadcrumbs (breadCrumbField)
import           Data.Monoid (mappend, mconcat)
import           Text.Blaze.Html.Renderer.String (renderHtml)
import           Data.List(intersperse)
import           System.FilePath.Posix (takeBaseName, takeDirectory)
import           Text.Blaze.Html                 (toHtml)

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
            breadCrumbField "breadcrumbs" `mappend`
            defaultContext
