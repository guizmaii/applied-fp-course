{-# LANGUAGE OverloadedStrings #-}
module Level05.DB
  ( FirstAppDB (FirstAppDB)
  , initDB
  , closeDB
  , addCommentToTopic
  , getComments
  , getTopics
  , deleteTopic
  ) where

import           Control.Monad.IO.Class             (liftIO)

import           Data.Text                          (Text)
import qualified Data.Text                          as Text

import           Data.Bifunctor                     (first, second)
import           Data.Time                          (UTCTime, getCurrentTime)

import           Database.SQLite.Simple             (Connection,
                                                     Query (fromQuery))
import qualified Database.SQLite.Simple             as Sql

import qualified Database.SQLite.SimpleErrors       as Sql
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           Level05.Types                      (Comment, CommentText,
                                                     Error (DBError), Topic,
                                                     fromDBComment,
                                                     getCommentText, getTopic,
                                                     mkTopic)

import           Level05.AppM                       (AppM (..))

import           Data.Functor                       ((<&>))
import           Data.Either.Combinators            (mapLeft)

import           Level05.Types.Error                (Error (..))

import           Level05.DB.Types                   (DBComment)

import           Database.SQLite.Simple             (Connection, Query (Query), Only (..), NamedParam (..))

import           Control.Monad                      (join)

runDBActionE :: IO a -> IO (Either Error a)
runDBActionE io = Sql.runDBAction io <&> mapLeft DBError

-- We have a data type to simplify passing around the information we need to run
-- our database queries. This also allows things to change over time without
-- having to rewrite all of the functions that need to interact with DB related
-- things in different ways.
newtype FirstAppDB = FirstAppDB
  { dbConn  :: Connection
  }

-- Quick helper to pull the connection and close it down.
closeDB :: FirstAppDB -> IO ()
closeDB = Sql.close . dbConn

initDB :: FilePath -> IO ( Either SQLiteResponse FirstAppDB )
initDB fp = Sql.runDBAction $ do
  -- Initialise the connection to the DB...
  -- - What could go wrong here?
  -- - What haven't we be told in the types?
  con <- Sql.open fp
  -- Initialise our one table, if it's not there already
  _ <- Sql.execute_ con createTableQ
  pure $ FirstAppDB con
  where
  -- Query has an `IsString` instance so string literals like this can be
  -- converted into a `Query` type when the `OverloadedStrings` language
  -- extension is enabled.
    createTableQ =
      "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time INTEGER)"

runDB :: (a -> Either Error b) -> IO a -> AppM b
runDB f ioea =
  -- This function is intended to abstract away the running of DB functions and
  -- the catching of any errors. As well as the process of running some
  -- processing function over those results.
 AppM $ runDBActionE ioea <&> (f =<<)
  -- Move your use of DB.runDBAction to this function to avoid repeating
  -- yourself in the various DB functions.

getComments :: FirstAppDB -> Topic -> AppM [Comment]
getComments db topic =
    let
      -- There are several possible implementations of this function. Particularly
      -- there may be a trade-off between deciding to throw an Error if a DBComment
      -- cannot be converted to a Comment, or simply ignoring any DBComment that is
      -- not valid.
      sql   = "SELECT id, topic, comment, time FROM comments WHERE topic = :topic"
      query = Sql.queryNamed (dbConn db) sql [":topic" := getTopic topic] :: IO [DBComment]
    in runDB (traverse fromDBComment) query

addCommentToTopic :: FirstAppDB -> Topic -> CommentText -> AppM ()
addCommentToTopic db topic comment =
    let
      sql       = "INSERT INTO comments (topic,comment,time) VALUES (?,?,?)"
      query now = Sql.execute (dbConn db) sql (getTopic topic, getCommentText comment, now :: UTCTime) :: IO ()
    in
      AppM $ runDBActionE $ getCurrentTime >>= query

getTopics :: FirstAppDB -> AppM [Topic]
getTopics db =
  let
    sql   = "SELECT DISTINCT topic FROM comments"
    query = (Sql.query_ (dbConn db) sql :: IO [Only Text]) <&> map fromOnly
  in
    runDB (traverse mkTopic) query

deleteTopic :: FirstAppDB -> Topic -> AppM ()
deleteTopic db topic =
  let
    t     = getTopic topic
    sql   = "DELETE FROM comments WHERE topic = :topic"
    query = Sql.executeNamed (dbConn db) sql [":topic" := t] :: IO ()
  in
    AppM $ runDBActionE query

-- Go to 'src/Level05/Core.hs' next.
