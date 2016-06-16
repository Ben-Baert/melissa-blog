{-# LANGUAGE OverloadedStrings #-}
module RelatedPosts 
    ( previousPostField,
      nextPostField,
      relatedPostsField
    ) where
import           Control.Applicative           (Alternative (..))
import           Hakyll
import           System.FilePath               (takeDirectory, dropExtension)
import           Control.Applicative           ((<$>))
import           Data.Maybe                    (fromMaybe, fromJust)

previousPostField :: String -> Context String 
previousPostField key = field key previousPost

nextPostField :: String -> Context String
nextPostField key = field key nextPost

toPattern :: Item String -> Pattern
toPattern =  fromGlob . (++ "/**") . takeDirectory . toFilePath . itemIdentifier

nPost :: Item String -> ([Identifier] -> Identifier -> Maybe Identifier) -> Compiler String
nPost post sf = do
    sameCategoryPosts <- sortChronological =<< getMatches =<< return (toPattern post)
    let pp = sf sameCategoryPosts (itemIdentifier post)
        in case pp of  
            Just x -> return ('/' : (dropExtension $ toFilePath x))
            Nothing -> empty 

previousPost :: Item String -> Compiler String
previousPost post = nPost post itemBefore

nextPost :: Item String -> Compiler String
nextPost post = nPost post itemAfter

itemBefore :: Eq a => [a] -> a -> Maybe a
itemBefore xs x =
    lookup x $ zip (tail xs) xs

itemAfter :: Eq a => [a] -> a -> Maybe a
itemAfter xs x =
    lookup x $ zip xs (tail xs)

relatedPostsField :: String -> Context a
relatedPostsField = undefined

