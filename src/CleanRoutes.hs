{-# LANGUAGE OverloadedStrings #-}
module CleanRoutes 
    ( cleanRoute,
      cleanIndexUrls,
      toUrlString
    ) where

import           Data.Char (toLower)
import           Data.List (isSuffixOf, intersperse, intercalate)
import           System.FilePath.Posix ((</>), takeBaseName, takeDirectory, splitFileName)
import           Hakyll (Routes, customRoute, Item, Compiler, replaceAll, toFilePath, withUrls)
--------------------------------------------------------------------------------

toUrlString :: String -> String
toUrlString = map (toLower . \x -> if x == ' ' then '-' else x)

cleanRoute :: Routes
cleanRoute = customRoute $ toUrlString . createIndexRoute
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
