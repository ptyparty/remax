{-# LANGUAGE OverloadedStrings #-}

-- Content keys for changed lines: a content-addressed scheme with a
-- new scheme tag so the two stores can never cross-contaminate.
--
-- A key is the repository, the file, the line exactly as the diff writes
-- it (leading + or - included, so adding a line and removing it are
-- different changes), and which copy of that text this is within the
-- file. Nothing positional goes in, so a change stays keyed when it
-- slides up or down the file, when the surrounding lines are rewritten,
-- and when hunk boundaries move.
module Remax.Key
  ( lineKeys
  , noteGroupId
  ) where

import Data.Digest.Pure.SHA qualified as SHA
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE

-- One key per marked changed line ("+text" or "-text"), in the order the
-- lines appear in the file's decorations. The copy ordinal keeps twins
-- apart: the second identical "+ foo" in a file is a different key from
-- the first, so reviewing one leaves the other pending.
lineKeys :: T.Text -> T.Text -> [T.Text] -> [T.Text]
lineKeys repo file markedLines =
  let
    lineId l =
      sha256Hex (T.intercalate "\NUL" ["remax/1", repo, file, l])
    go _ [] = []
    go seen (l : rest) =
      let
        lid = lineId l
        ordinal = Map.findWithDefault (0 :: Int) lid seen
        key = sha256Hex (T.concat [lid, "\NUL", T.pack (show ordinal)])
      in
        key : go (Map.insert lid (ordinal + 1) seen) rest
  in
    go Map.empty markedLines

-- One name for the lines a note was written across.
--
-- A note is stored under every key of its group, so something has to be
-- able to find the siblings again. Comparing what the notes *say* was
-- that something, and two notes worded the same — "typo", "why?" — were
-- indistinguishable from one note spanning both places: an agent's
-- reply to one landed on all of them, and the same conversation
-- appeared under hunks nobody had asked about. So the group is named
-- instead, from the keys it covers, and the name is what is stored.
--
-- Derived from the keys rather than drawn at random so that noting the
-- same lines again continues the group it continued before, and so that
-- nothing here needs a source of entropy.
noteGroupId :: T.Text -> [T.Text] -> T.Text
noteGroupId repo keys =
  sha256Hex (T.intercalate "\NUL" ("remax/group/1" : repo : List.sort keys))

sha256Hex :: T.Text -> T.Text
sha256Hex =
  T.pack . SHA.showDigest . SHA.sha256 . TLE.encodeUtf8 . TL.fromStrict
