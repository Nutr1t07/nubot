{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Util.Misc where

import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Lazy.Search   as BL
import qualified Data.List                     as List
import           Data.String                    ( IsString )
import qualified Data.Text                     as Text
import qualified Control.Arrow                 as Arrow (first)
import qualified Data.Tuple                    as Tup (swap)
import qualified Network.URI                   as URI (
                                                escapeURIString
                                              , isUnescapedInURI )
                                              
showT :: forall a. Show a => a -> Text.Text
showT = Text.pack . show

encodeURI :: String -> String
encodeURI = URI.escapeURIString predi
  where
    predi '%' = True
    predi x = URI.isUnescapedInURI x

checkEmpty :: (Monoid a, Eq a) => a -> Maybe a
checkEmpty txt = if txt == mempty then Nothing else Just txt

searchBetweenBL
  :: BS.ByteString -> BS.ByteString -> BL.ByteString -> Maybe BL.ByteString
searchBetweenBL left right content =
  let fstround = snd $ BL.breakAfter left content
  in  checkEmpty $ fst (BL.breakOn right fstround)

searchBetweenText :: Text.Text -> Text.Text -> Text.Text -> Maybe Text.Text
searchBetweenText left right content =
  let fstround = snd $ Text.breakOn left content
  in  checkEmpty $ fst (Text.breakOn right fstround)

searchAllBetweenBL
  :: BS.ByteString -> BS.ByteString -> BL.ByteString -> [BL.ByteString]
searchAllBetweenBL _ _ "" = []
searchAllBetweenBL left right content =
  let matchLeft  = snd $ BL.breakAfter left content
      matchRight = BL.breakOn right matchLeft
  in  if fst matchRight /= ""
        then fst matchRight : searchAllBetweenBL left right (snd matchRight)
        else searchAllBetweenBL left right (snd matchRight)

unlines :: (Monoid a, IsString a) => [a] -> a
unlines = mconcat . List.intersperse "\n"

maybe' :: Maybe a -> c -> (a -> c) -> c
maybe' = flip $ flip <$> maybe

either' :: Either a b -> (a -> c) -> (b -> c) -> c
either' = flip $ flip <$> either

breakOn :: Eq a => [a] -> [a] -> ([a], [a])
breakOn needle haystack | needle `List.isPrefixOf` haystack = ([], haystack)
breakOn needle [] = ([], [])
breakOn needle (x:xs) = Arrow.first (x:) $ breakOn needle xs

breakOnEnd :: Eq a => [a] -> [a] -> ([a], [a])
breakOnEnd needle haystack = both reverse $ Tup.swap $ breakOn (reverse needle) (reverse haystack)
  where 
    both f (x,y) = (f x, f y)