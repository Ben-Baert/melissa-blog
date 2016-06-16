{-# LANGUAGE OverloadedStrings #-}
module PrettyCategory
    ( prettyCategory
    ) where

--------------------------------------------------------------------------------

import           Data.Char(toUpper)
--------------------------------------------------------------------------------
replSpaces :: String -> String
replSpaces = map (\x -> if x == ' ' then '-' else x)

replDash :: String -> String
replDash = map (\x -> if x == '-' then ' ' else x)

title :: String -> String
title = unwords .  map firstUpper  . words
    where firstUpper (x:xs) = toUpper x : xs

prettyCategory :: String -> String
prettyCategory = title . replDash
