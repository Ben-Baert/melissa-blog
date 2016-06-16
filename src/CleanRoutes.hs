{-# LANGUAGE OverloadedStrings #-}
module CleanRoutes 
    ( cleanRoute,
      cleanIndexUrls
    ) where

import           Data.List (isSuffixOf, intersperse, intercalate)
import           System.FilePath.Posix ((</>), takeBaseName, takeDirectory, splitFileName)
import           Hakyll (Routes, customRoute, Item, Compiler, replaceAll, toFilePath, withUrls)
--------------------------------------------------------------------------------

replSpaces :: String -> String
replSpaces = map (\x -> if x == ' ' then '-' else x)

cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute
  where
      createIndexRoute ident = replSpaces $ takeDirectory p </> takeBaseName p </> "index.html"
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
