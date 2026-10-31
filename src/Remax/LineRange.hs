{-# LANGUAGE OverloadedStrings #-}

module Remax.LineRange
  ( Lines (..)
  , linesContain
  , linesText
  , LineRange (..)
  , parseLineRange
  , lineRangeText
  ) where

import Control.Monad qualified as Monad
import Data.Text qualified as T
import Remax.Git qualified as Git

data Lines = Lines
  { lnFirst :: Int
  , lnLast :: Int
  }
  deriving (Eq)

linesContain :: Lines -> Int -> Bool
linesContain ls n = n >= lnFirst ls && n <= lnLast ls

linesText :: Lines -> T.Text
linesText ls =
  if lnFirst ls == lnLast ls
    then T.pack (show (lnFirst ls))
    else T.pack (show (lnFirst ls)) <> "-" <> T.pack (show (lnLast ls))

data LineRange = LineRange
  { lrPath :: Maybe T.Text
  , lrLines :: Lines
  }

parseLineRange :: T.Text -> Maybe LineRange
parseLineRange word =
  let
    (before, after) = T.breakOnEnd ":" word
    (from, to) = T.breakOn "-" after
    path = T.dropEnd 1 before
  in
    do
      Monad.guard (T.null before || not (T.null path))
      first <- Git.readCount from
      final <- if T.null to then Just first else Git.readCount (T.drop 1 to)
      Monad.guard (min first final >= 1)
      Just
        LineRange
          { lrPath = if T.null before then Nothing else Just path
          , lrLines = Lines {lnFirst = min first final, lnLast = max first final}
          }

lineRangeText :: LineRange -> T.Text
lineRangeText range =
  maybe (linesText (lrLines range)) (\path -> path <> ":" <> linesText (lrLines range)) (lrPath range)
