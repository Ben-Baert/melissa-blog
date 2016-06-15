{-# LANGUAGE OverloadedStrings #-}
module RelatedPosts 
    ( previousPostField,
      nextPostField,
      relatedPostsField
    ) where
import Hakyll
import System.FilePath(takeDirectory)
import Control.Applicative ((<$>))


sameCategory :: FilePath -> FilePath -> Bool
sameCategory x y = takeDirectory x == takeDirectory y

--find posts in same category, select previous and next
previousPostField :: String -> Context String 
previousPostField key = field key previousPost


nextPostField :: String -> Context String
nextPostField key = field key nextPost

toPattern :: Item String -> Pattern
toPattern =  fromGlob . (++ "/**") . takeDirectory . toFilePath . itemIdentifier

previousPost :: Item String -> Compiler String
previousPost post = do
    sameCategoryPosts <- sortChronological =<< getMatches =<< return (toPattern post)
    let pp = itemBefore sameCategoryPosts (itemIdentifier post)
        in case pp of
            Just x -> loadBody x
            Nothing -> return("?") 

nextPost :: Item String -> Compiler String
nextPost = undefined

itemBefore :: Eq a => [a] -> a -> Maybe a
itemBefore xs x =
    lookup x $ zip (tail xs) xs

relatedPostsField :: String -> Context a
relatedPostsField = undefined

