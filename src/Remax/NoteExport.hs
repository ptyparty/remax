{-# LANGUAGE OverloadedStrings #-}

module Remax.NoteExport
  ( Which (..)
  , exportNotes
  , notesJson
  ) where

import Data.List qualified as List
import Data.Text qualified as T
import Database.SQLite.Simple qualified as SQLite
import Remax.Json qualified as Json
import Remax.NoteArchive qualified as NoteArchive
import Remax.WorkStore qualified as WorkStore

data Which = AllNotes | RetiredOnly
  deriving (Eq, Show)

exportNotes :: SQLite.Connection -> Which -> IO [NoteArchive.ExportedNote]
exportNotes conn which = do
  (open, archived) <-
    SQLite.withTransaction conn $ do
      archived <- NoteArchive.archivedNotes conn
      open <- case which of
        AllNotes -> NoteArchive.liveNotes conn Nothing
        RetiredOnly -> pure []
      pure (open, archived)
  pure (List.sortOn order (map stamped (open ++ archived)))

order :: NoteArchive.ExportedNote -> (Bool, T.Text, T.Text, T.Text)
order n = (null (NoteArchive.notePosts n), NoteArchive.noteStarted n, NoteArchive.noteRepo n, NoteArchive.noteId n)

stamped :: NoteArchive.ExportedNote -> NoteArchive.ExportedNote
stamped n =
  n
    { NoteArchive.noteState = case NoteArchive.noteState n of
        NoteArchive.Open -> NoteArchive.Open
        NoteArchive.Retired at -> NoteArchive.Retired (WorkStore.isoStamp at)
    , NoteArchive.notePosts = map (\p -> p {NoteArchive.postAt = WorkStore.isoStamp (NoteArchive.postAt p)}) (NoteArchive.notePosts n)
    }

notesJson :: [NoteArchive.ExportedNote] -> T.Text
notesJson = Json.encode . Json.JArray . map noteJson

noteJson :: NoteArchive.ExportedNote -> Json.Json
noteJson n =
  let
    optional = maybe Json.JNull Json.JString
    stateFields = case NoteArchive.noteState n of
      NoteArchive.Open -> [("state", Json.JString "open"), ("retired_at", Json.JNull)]
      NoteArchive.Retired at -> [("state", Json.JString "retired"), ("retired_at", Json.JString at)]
    lineJson l =
      Json.JObject
        [ ("line_no", maybe Json.JNull (Json.JNumber . T.pack . show) (NoteArchive.lineNumber l))
        , ("text", Json.JString (NoteArchive.lineText l))
        ]
    postJson p =
      Json.JObject
        [ ("kind", Json.JString (NoteArchive.postKind p))
        , ("author", Json.JString (NoteArchive.postAuthor p))
        , ("body", Json.JString (NoteArchive.postBody p))
        , ("at", Json.JString (NoteArchive.postAt p))
        ]
  in
    Json.JObject
      ( [("id", Json.JString (NoteArchive.noteId n)), ("repo", Json.JString (NoteArchive.noteRepo n))]
          ++ stateFields
          ++ [ ("file", optional (NoteArchive.noteFile n))
             , ("side", optional (NoteArchive.noteSide n))
             , ("verdict", optional (NoteArchive.noteVerdict n))
             , ("ref", optional (NoteArchive.noteRef n))
             , ("commit", optional (NoteArchive.noteCommit n))
             , ("lines", Json.JArray (map lineJson (NoteArchive.noteLines n)))
             , ("posts", Json.JArray (map postJson (NoteArchive.notePosts n)))
             ]
      )
