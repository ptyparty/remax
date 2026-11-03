{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Remax.Guide
  ( GuideError (..)
  , renderGuideError
  , guidePage
  ) where

import Data.ByteString qualified as BS
import Data.FileEmbed qualified as FileEmbed
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import System.FilePath qualified as FilePath

newtype GuideError = NoSuchPage T.Text

renderGuideError :: GuideError -> T.Text
renderGuideError e =
  case e of
    NoSuchPage name ->
      "remax guide has no page "
        <> name
        <> "; its pages are: "
        <> T.intercalate ", " (fmap fst guidePages)

guidePage :: T.Text -> Either GuideError T.Text
guidePage requested =
  let
    name = pageName requested
  in
    maybe (Left (NoSuchPage name)) Right (lookup name guidePages)

pageName :: T.Text -> T.Text
pageName requested =
  let
    withoutAnchor = T.takeWhile (/= '#') requested
    withoutDir = T.pack . FilePath.takeFileName . T.unpack $ withoutAnchor
  in
    Maybe.fromMaybe withoutDir (T.stripSuffix ".md" withoutDir)

guidePages :: [(T.Text, T.Text)]
guidePages =
  List.sortOn fst (fmap pageEntry embeddedPages)

pageEntry :: (FilePath, BS.ByteString) -> (T.Text, T.Text)
pageEntry (path, body) =
  (T.pack (FilePath.dropExtension path), TE.decodeUtf8 body)

embeddedPages :: [(FilePath, BS.ByteString)]
embeddedPages =
  List.filter ((== ".md") . FilePath.takeExtension . fst) $(FileEmbed.embedDir "guide")
