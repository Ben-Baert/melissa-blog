{-# LANGUAGE OverloadedStrings #-}
module Context 
    ( teaserCtx,
      categoryCtx,
      defaultCtx
    ) where

import           Hakyll
import           PrettyCategory
import           Breadcrumbs                     (breadCrumbField)
import           Data.Monoid                     ((<>))
import           Text.Blaze.Html.Renderer.String (renderHtml)
import           Data.List                       (intersperse)
import           Text.Blaze.Html                 (toHtml, toValue, (!))
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A
import           System.FilePath.Posix           (takeBaseName, takeDirectory)
import           Data.Maybe                      (fromMaybe)
import           CleanRoutes (toUrlString)

authorField :: Context String
authorField = field "author" $ \item -> do
    authors <- getMetadataField (itemIdentifier item) "authors"
    text <- return $ fromMaybe "Melissa Katon" authors
    let path = "/author/" ++ text
        in return $ renderHtml $ H.a ! A.href (toValue $ toUrlString $ path) $ toHtml $ prettyCategory $ text 

teaserCtx :: Context String
teaserCtx = 
        teaserField "teaser" "content" <> 
        defaultCtx

categoryField' :: String -> Tags -> Context a
categoryField' = 
    tagsFieldWith getCategory render (mconcat . intersperse ", ")
        where 
            render tag _ = Just $ toHtml $ prettyCategory tag
            getCategory = return . return . takeBaseName . takeDirectory . toFilePath



categoryCtx :: Tags -> Tags -> Context String
categoryCtx tags category =
        categoryField' "category" category <>
        tagsField     "tags"     tags     <>
        teaserCtx

defaultCtx :: Context String
defaultCtx = 
            authorField                   <>
            breadCrumbField "breadcrumbs" <>
            defaultContext
