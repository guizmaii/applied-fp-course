{-# LANGUAGE OverloadedStrings #-}
module Level04.Types.Error
  ( Error(..)
  , nonEmptyText
  , toDBError
  ) where

import Data.Text                            (Text)
import Database.SQLite.SimpleErrors.Types   (SQLiteResponse)

import Data.Either.Combinators              (mapLeft)

data Error
  = UnknownRoute
  | EmptyCommentText
  | EmptyTopic
  | DBError SQLiteResponse
  -- Add another constructor for our DB error types.
  deriving (Eq, Show)

nonEmptyText :: (Text -> a) -> Error -> Text -> Either Error a
nonEmptyText _ e "" = Left e
nonEmptyText c _ tx = Right (c tx)

toDBError :: Either SQLiteResponse a -> Either Error a
toDBError = mapLeft DBError