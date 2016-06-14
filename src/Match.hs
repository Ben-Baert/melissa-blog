{-# LANGUAGE OverloadedStrings #-}
module Match 
    ( match
    ) where

import Hakyll (Metadata, Pattern, Rules, matchMetadata) 
import qualified Data.Map as Map


metadataFieldIs :: String -> String -> Metadata -> Bool
metadataFieldIs key value metadata = case Map.lookup key metadata of
                                        Just v  -> value == v
                                        Nothing -> True


isPublic :: Metadata -> Bool
isPublic = metadataFieldIs "draft" "false"


match :: Pattern -> Rules () -> Rules ()
match pattern = matchMetadata pattern isPublic

