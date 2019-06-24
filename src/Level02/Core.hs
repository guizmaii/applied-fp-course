{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
module Level02.Core (runApp, app) where

import           Network.Wai              (Application, Request (..), Response,
                                           pathInfo, requestMethod, responseLBS,
                                           strictRequestBody)
import           Network.Wai.Handler.Warp (run)

import           Network.HTTP.Types       (Status, hContentType, status200,
                                           status400, status404)

import qualified Data.ByteString.Lazy     as LBS
import          Data.ByteString.Char8     (pack)

import           Data.Either              (either)

import           Data.Text                (Text)
import           Data.Text.Encoding       (decodeUtf8)

import           Level02.Types            (ContentType (..), Error (..), RqType (..),
                                           mkCommentText, mkTopic, getCommentText, getTopic,
                                           renderContentType)

-- |-------------------------------------------|
-- |- Don't start here, go to Level02.Types!  -|
-- |-------------------------------------------|

-- | Some helper functions to make our lives a little more DRY.
mkResponse :: Status -> ContentType -> LBS.ByteString -> Response
mkResponse status contentType = responseLBS status [("Content-Type", renderContentType contentType)]

resp200 :: ContentType -> LBS.ByteString -> Response
resp200 = mkResponse status200

resp404 :: ContentType -> LBS.ByteString -> Response
resp404 = mkResponse status404

resp400 :: ContentType -> LBS.ByteString -> Response
resp400 = mkResponse status400

-- |----------------------------------------------------------------------------------
-- These next few functions will take raw request information and construct         --
-- one of our types.                                                                --
--                                                                                  --
-- By breaking out these smaller functions, we're able to isolate our               --
-- validation requirements into smaller components that are simpler to maintain     --
-- and verify. It also allows for greater reuse and it also means that              --
-- validation is not duplicated across the application, maybe incorrectly.          --
--------------------------------------------------------------------------------------

decodeByteString :: LBS.ByteString -> Text
decodeByteString = decodeUtf8 . LBS.toStrict

-- TODO Jules: Rewrite with Applicative
mkAddRequest :: Text -> LBS.ByteString -> Either Error RqType
mkAddRequest topicTxt rawComment = do
  topic   <- mkTopic topicTxt
  comment <- mkCommentText $ decodeByteString rawComment
  Right $ AddRq topic comment

mkViewRequest :: Text -> Either Error RqType
mkViewRequest topicTxt = ViewRq <$> mkTopic topicTxt

mkListRequest :: Either Error RqType
mkListRequest = Right ListRq

-- |----------------------------------
-- end of RqType creation functions --
--------------------------------------

mkErrorResponse :: Error -> Response
mkErrorResponse EmptyTopicMessageError    = resp400 PlainText "Empty Topic text"
mkErrorResponse EmptyCommentMessageError  = resp400 PlainText "Empty Comment text"
mkErrorResponse NotFound                  = resp404 PlainText "Not Found"

-- | Use our ``RqType`` helpers to write a function that will take the input
-- ``Request`` from the Wai library and turn it into something our application
-- cares about.
-- TODO Jules: How do you extract data from URL ?
mkRequest :: Request -> IO ( Either Error RqType )
mkRequest request =
  return $ case (requestMethod request, rawPathInfo request) of
    ("GET", "/list")        -> mkListRequest
    ("GET", "/topic/view")  -> mkViewRequest "Toto"
    ("POST", "/topic/add")  -> mkAddRequest "Tata" "Tutu"
    _                       -> Left NotFound

-- | If we find that we need more information to handle a request, or we have a
-- new type of request that we'd like to handle then we update the ``RqType``
-- structure and the compiler will let us know which parts of our application
-- are affected.
--
-- Reduction of concerns such that each section of the application only deals
-- with a small piece is one of the benefits of developing in this way.
--
-- For now, return a made-up value for each of the responses as we don't have
-- any persistent storage. Plain text responses that contain "X not implemented
-- yet" should be sufficient.
handleRequest :: RqType -> Either Error Response
handleRequest (AddRq _ _) = Right $ resp200 PlainText "Topic: toto, Comment: tata"
handleRequest (ViewRq _)  = Right $ resp200 PlainText "Topic: tutu"
handleRequest ListRq      = Right $ resp200 PlainText "Topics: a, b, c"

buildAnswer :: Either Error RqType -> Either Error Response
buildAnswer rq = rq >>= handleRequest

-- TODO Jules: Find a better name
-- TODO Jules: Should be written with `fmap` and not `bind`
-- TODO Jules: Can I replace the lambda case by a `fold` or `mapBoth` ?
tutu :: IO ( Either Error Response ) -> IO Response
tutu resp = resp >>= \case
    Right r -> return r
    Left e  -> return $ mkErrorResponse e

-- | Reimplement this function using the new functions and ``RqType`` constructors as a guide.
-- type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
-- TODO Jules: Super ugly! How to improve that ?
app :: Application
app req cb = tutu (buildAnswer <$> mkRequest req) >>= cb

runApp :: IO ()
runApp = run 3000 app
