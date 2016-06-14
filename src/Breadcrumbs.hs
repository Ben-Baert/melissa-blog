{-# LANGUAGE OverloadedStrings #-}
module Breadcrumbs
    ( breadCrumbField
    ) where

import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A
import           Data.List (intersperse)
import           Text.Blaze.Html                 (toHtml, toValue, (!))
import           Text.Blaze.Html.Renderer.String (renderHtml)
import           PrettyCategory
import           System.FilePath                 ((</>), takeBaseName, takeDirectory, splitFileName, dropExtension)
import           Hakyll                          (Context, field, toFilePath, itemIdentifier)
--------------------------------------------------------------------------------

crumbs :: FilePath -> [H.Html]
crumbs "/" = []
crumbs "." = []
-- full url/actual url; get rid of full url
crumbs fp = (H.a ! A.href (toValue ("/" ++fp)) $ toHtml $ prettyCategory $ takeBaseName fp) : (crumbs (takeDirectory fp))

completeCrumbs :: [H.Html] -> H.Html
completeCrumbs crumbs =
                mconcat $ (intersperse (toHtml (" > " :: String) )) $ homeLink : (reverse crumbs)
                    where homeLink = H.a ! A.href (toValue ("/" :: String)) $ toHtml ("Home" :: String) 

breadCrumbField :: String -> Context a
breadCrumbField key = field key $ return . renderHtml . completeCrumbs . crumbs . dropExtension . toFilePath . itemIdentifier
