{-# LANGUAGE OverloadedStrings #-}
module Utils
    ( getAuthors,
      postPattern) where
import           Data.Maybe             (fromMaybe)
import           Hakyll     hiding      (match)
--------------------------------------------------------------------------------

getMetadataFieldAsList :: MonadMetadata m => Identifier -> String -> String -> m [String]
getMetadataFieldAsList identifier fieldname d = do
    f <- getMetadataField identifier fieldname
    return $ fromMaybe [d] $ (map trim . splitAll ",") <$> f

getAuthors :: MonadMetadata m => Identifier -> m [String]
getAuthors identifier = getMetadataFieldAsList identifier "authors" "Melissa Katon"

postPattern :: Pattern
postPattern = "blog/**.md" .||. "travel/*/day-*.md" 
