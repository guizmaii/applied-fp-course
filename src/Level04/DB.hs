{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Level04.DB
  ( FirstAppDB (FirstAppDB)
  , initDB
  , exceptionalInitDB
  , closeDB
  , addCommentToTopic
  , getComments
  , getTopics
  , deleteTopic
  ) where

import           Data.Text                          (Text)
import qualified Data.Text                          as Text

import           Data.Time                          (getCurrentTime)

import           Database.SQLite.Simple             (Connection, Query (Query), Only (..), NamedParam (..))
import qualified Database.SQLite.Simple             as Sql

import qualified Database.SQLite.SimpleErrors       as Sql
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           Level04.Types                      (Comment, CommentText,
                                                     Error, Topic, getTopic, getCommentText, fromDBComment, mkTopic)

import           Level04.Types.Error                (Error (..), toDBError)

import           Level04.DB.Types                   (toTopic)
import           Database.SQLite.Simple.FromRow

import           Database.SQLite.SimpleErrors       (runDBAction)

import           Level04.DB.Types                   (DBComment)

import           Data.Time.Clock                    (UTCTime, getCurrentTime)

import           Data.Functor                       ((<&>))
import           Control.Monad                      (join)

import           GHC.IO                             (throwIO)
import           Data.Either                        (either)

-- ------------------------------------------------------------------------|
-- You'll need the documentation for sqlite-simple ready for this section! |
-- ------------------------------------------------------------------------|

-- We have a data type to simplify passing around the information we need to run
-- our database queries. This also allows things to change over time without
-- having to rewrite all of the functions that need to interact with DB related
-- things in different ways.
--
-- To help with that, we create a new data type that can hold our `Connection`
-- for us, and allows it to be expanded later if we need to
data FirstAppDB = FirstAppDB
  { dbConn :: Connection
  }

runDBActionE :: IO a -> IO (Either Error a)
runDBActionE io = runDBAction io <&> toDBError

-- Quick helper to pull the connection and close it down.
closeDB :: FirstAppDB -> IO ()
closeDB db = Sql.close $ dbConn db

-- Given a `FilePath` to our SQLite DB file, initialise the database and ensure
-- our Table is there by running a query to create it, if it doesn't exist
-- already.
initDB :: FilePath -> IO ( Either SQLiteResponse FirstAppDB )
initDB fp =
  runDBAction doCreate
  where
    doCreate = do
      conn <- Sql.open fp
      Sql.execute_ conn createTableQ
      return $ FirstAppDB conn

    -- Query has an `IsString` instance so string literals like this can be
    -- converted into a `Query` type when the `OverloadedStrings` language
    -- extension is enabled.
    createTableQ = "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time TEXT)"

exceptionalInitDB :: FilePath -> IO FirstAppDB
exceptionalInitDB fp = initDB fp >>= either throwIO pure

-- Note that we don't store the `Comment` in the DB, it is the type we build
-- to send to the outside world. We will be loading our `DBComment` type from
-- the FirstApp.DB.Types module before converting trying to convert it to a
-- `Comment`.
--
-- To go from a DBComment to a Comment, we need to use ``fromDBComment`` that is
-- defined in FirstApp.Types.
--
-- HINT: You can use '?' or named place-holders as query parameters. Have a look
-- at the section on parameter substitution in sqlite-simple's documentation.
-- TODO Jules: TO TEST!
getComments :: FirstAppDB -> Topic -> IO (Either Error [Comment])
getComments db topic =
  let
    -- There are several possible implementations of this function. Particularly
    -- there may be a trade-off between deciding to throw an Error if a DBComment
    -- cannot be converted to a Comment, or simply ignoring any DBComment that is
    -- not valid.
    sql   = "SELECT id, topic, comment, time FROM comments WHERE topic = :topic"
    query = Sql.queryNamed (dbConn db) sql [":topic" := getTopic topic] :: IO [DBComment]
  in runDBActionE query <&> flatTraverse fromDBComment

addCommentToTopic :: FirstAppDB -> Topic -> CommentText -> IO (Either Error ())
addCommentToTopic db topic comment =
  let
    sql       = "INSERT INTO comments (topic,comment,time) VALUES (?,?,?)"
    query now = Sql.execute (dbConn db) sql (getTopic topic, getCommentText comment, now :: UTCTime) :: IO ()
  in
    runDBActionE (query =<< getCurrentTime)

-- TODO Jules: Find how to write this function!
-- biTraverse :: Bifunctor m => (a -> m e b) -> (c -> m e a) -> m c [a] -> m e [b]

flatTraverse :: (Monad m, Traversable t) => (a -> m b) -> m (t a) -> m (t b)
flatTraverse f e = traverse f =<< e

getTopics :: FirstAppDB -> IO (Either Error [Topic])
getTopics db =
  let
    sql   = "SELECT DISTINCT topic FROM comments"
    query = (Sql.query_ (dbConn db) sql :: IO [Only Text]) <&> map fromOnly
  in
    runDBActionE query <&> flatTraverse mkTopic

deleteTopic :: FirstAppDB -> Topic -> IO (Either Error ())
deleteTopic db topic =
  let
    t     = getTopic topic
    sql   = "DELETE FROM comments WHERE topic = :topic"
    query = Sql.executeNamed (dbConn db) sql [":topic" := t] :: IO ()
  in
    runDBActionE query
