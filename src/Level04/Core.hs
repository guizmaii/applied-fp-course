{-# LANGUAGE OverloadedStrings #-}
module Level04.Core
  ( runApp
  , app
  ) where

import           Control.Applicative                (liftA2)
import           Control.Monad                      (join)

import           Network.Wai                        (Application, Request,
                                                     Response, pathInfo,
                                                     requestMethod, responseLBS,
                                                     strictRequestBody)
import           Network.Wai.Handler.Warp           (run)

import           Network.HTTP.Types                 (Status, hContentType,
                                                     status200, status400,
                                                     status404, status500)

import qualified Data.ByteString.Lazy.Char8         as LBS

import           Data.Either                        (Either (Left, Right),
                                                     either)

import           Data.Semigroup                     ((<>))
import           Data.Text                          (Text)
import           Data.Text.Encoding                 (decodeUtf8)
import           Data.Text.Lazy.Encoding            (encodeUtf8)

import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           Waargonaut.Encode                  (Encoder')
import qualified Waargonaut.Encode                  as E

import           Level04.Conf                       (Conf, firstAppConfig)
import qualified Level04.DB                         as DB
import           Level04.Types                      (ContentType (JSON, PlainText),
                                                     Error (..),
                                                     RqType (AddRq, ListRq, ViewRq),
                                                     mkCommentText, mkTopic,
                                                     renderContentType)

import           Data.Functor                       ((<&>))

import           Level04.DB                         (initDB, exceptionalInitDB, closeDB)
import           GHC.IO                             (bracket)

import qualified Level04.Conf                       as Conf
import           Level04.Conf                       (Conf (..))

import           Data.Either.Combinators            (mapLeft)
import           Data.Bifunctor

import           Level04.Types                      (Comment, encodeComment)
import           Level04.Types.Topic                (encodeTopic)

import           Waargonaut.Encode                  (simplePureEncodeByteString)

-- Our start-up is becoming more complicated and could fail in new and
-- interesting ways. But we also want to be able to capture these errors in a
-- single type so that we can deal with the entire start-up process as a whole.
data StartUpError = DBInitErr SQLiteResponse
  deriving Show

runApp :: IO ()
runApp =
  let
    acquire  = exceptionalInitDB $ dbFilePath Conf.firstAppConfig
    release  = closeDB
    appToRun = run 8080 . app
  in bracket acquire release appToRun

-- | Some helper functions to make our lives a little more DRY.
mkResponse
  :: Status
  -> ContentType
  -> LBS.ByteString
  -> Response
mkResponse sts ct =
  responseLBS sts [(hContentType, renderContentType ct)]

resp200
  :: ContentType
  -> LBS.ByteString
  -> Response
resp200 =
  mkResponse status200

resp404
  :: ContentType
  -> LBS.ByteString
  -> Response
resp404 =
  mkResponse status404

resp400
  :: ContentType
  -> LBS.ByteString
  -> Response
resp400 =
  mkResponse status400

-- Some new helpers for different statuses and content types
resp500
  :: ContentType
  -> LBS.ByteString
  -> Response
resp500 =
  mkResponse status500

resp200Json
  :: Encoder' a
  -> a
  -> Response
resp200Json e =
  mkResponse status200 JSON . encodeUtf8 .
  E.simplePureEncodeTextNoSpaces e

-- |
app
  :: DB.FirstAppDB -- ^ Add the Database record to our app so we can use it
  -> Application
app db rq cb = do
  rq'  <- mkRequest rq
  resp <- handleRErr rq' <&> handleRespErr
  cb resp
  where
    handleRespErr :: Either Error Response -> Response
    handleRespErr = either mkErrorResponse id

    -- We want to pass the Database through to the handleRequest so it's
    -- available to all of our handlers.
    handleRErr :: Either Error RqType -> IO (Either Error Response)
    handleRErr = either ( pure . Left ) ( handleRequest db )


-- TODO Jules: rewrite with 2 fmap
(**>) :: (Functor m, Bifunctor t) => m (t e a) -> (a -> b) -> m (t e b)
(**>) c f = c <&> second f

-- | Handle each of the different types of request. See how the types have helped narrow our focus
-- to only those types of request that we care about. Along with ensuring that once the data has
-- reached this point, we don't have to continually check if it is valid or usable. The types and
-- data structures that we created have taken care of that for us at an earlier stage, simplifying
-- this function.
--
-- For both the 'ViewRq' and 'ListRq' functions, we'll need to pass the correct 'Encoder' to the
-- 'resp200Json' function.
handleRequest :: DB.FirstAppDB -> RqType -> IO (Either Error Response)
handleRequest _db (AddRq t c) = DB.addCommentToTopic _db t c **> \_ -> resp200 PlainText "Success"
handleRequest _db (ViewRq t)  = DB.getComments _db t **> resp200Json (E.list encodeComment)
handleRequest _db ListRq      = DB.getTopics _db **> resp200Json (E.list encodeTopic)

mkRequest
  :: Request
  -> IO ( Either Error RqType )
mkRequest rq =
  case ( pathInfo rq, requestMethod rq ) of
    -- Commenting on a given topic
    ( [t, "add"], "POST" ) -> mkAddRequest t <$> strictRequestBody rq
    -- View the comments on a given topic
    ( [t, "view"], "GET" ) -> pure ( mkViewRequest t )
    -- List the current topics
    ( ["list"], "GET" )    -> pure mkListRequest
    -- Finally we don't care about any other requests so throw your hands in the air
    _                      -> pure ( Left UnknownRoute )

mkAddRequest :: Text -> LBS.ByteString -> Either Error RqType
mkAddRequest ti c = AddRq
  <$> mkTopic ti
  <*> (mkCommentText . decodeUtf8 . LBS.toStrict) c

mkViewRequest :: Text -> Either Error RqType
mkViewRequest = fmap ViewRq . mkTopic

mkListRequest :: Either Error RqType
mkListRequest = Right ListRq

mkErrorResponse :: Error -> Response
mkErrorResponse UnknownRoute     = resp404 PlainText "Unknown Route"
mkErrorResponse EmptyCommentText = resp400 PlainText "Empty Comment"
mkErrorResponse EmptyTopic       = resp400 PlainText "Empty Topic"
mkErrorResponse (DBError _)      = resp500 PlainText "An Error Occured"
