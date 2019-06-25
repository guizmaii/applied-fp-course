{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Level05.AppM
  ( AppM
  , liftEither
  , runAppM
  ) where

import           Control.Monad.Except    (MonadError (..))
import           Control.Monad.IO.Class  (MonadIO (..))

import           Data.Text               (Text)

import           Level05.Types           (Error)

import           Data.Bifunctor          (second)

import           Data.Functor            ((<&>))

-- We're going to add a very useful abstraction to our application. We'll
-- automate away the explicit error handling and inspection of our Either values
-- while preserving the type-level information that tells us what can go wrong.
--
-- To do this we will create a newtype `AppM` that is a shorthand way of
-- describing the return type of a function that may contain an error.
--
-- Our `AppM` type will work in the same manner as the Functor/Applicative/Monad
-- instances for Either, with functions being applied to the Right value and
-- everything been ignored if a Left value is encountered, returning that Left
-- value. With the added bonus of allowing us to perform `IO` actions!
--
-- f <$> (Left e)  = Left e
-- f <$> (Right a) = Right (f a)
--
-- (Left e)  >>= f = Left e
-- (Right a) >>= f = f a
--
-- This means when we have a function doing this sort of shuffling:
--
-- foo :: IO (Either Error Value)
-- foo = do
--   aE <- mightFail
--   either (pure . Left) needsAButMightFail aE
--   where
--     mightFail :: IO (Either Error Int)
--     needsAButMightFail :: Int -> IO (Either Error Value)
--
-- We can wrap our functions with AppM and we can work directly with the
-- values we expect to appear on the happy path, knowing that if the sad path is
-- encountered, the structure of our AppM will automatically handle it for us.

newtype AppM a = AppM (IO (Either Error a))
-- This structure allows us to start writing our functions in terms of
-- constraints. As an example, if we wanted to abstract over IO and indicate
-- that instead of the concrete type we wanted a constraint that allows for IO
-- actions. Our AppM would look more like this:
--
-- AppM m a = AppM ( m (Either Error a) )
--
-- Then our functions would look like:
--
-- foo :: MonadIO m => Int -> AppM m a
--
-- Or we could not use a concrete type for Error
--
-- AppM e m a = AppM ( m (Either e a) )

runAppM :: AppM a -> IO (Either Error a)
runAppM (AppM m) = m

instance Functor AppM where
  fmap :: (a -> b) -> AppM a -> AppM b
  fmap f (AppM ioea) = AppM $ ioea <&> second f
  -- teacher version: fmap f = AppM . (fmap . fmap) f . runAppM

instance Applicative AppM where
  pure :: a -> AppM a
  pure a = AppM $ pure $ pure a

  (<*>) :: AppM (a -> b) -> AppM a -> AppM b
  (<*>) (AppM f) (AppM a) = AppM $ (f <&> (<*>)) <*> a

  -- Teacher version
  -- (<*>) (AppM f) (AppM a) = AppM $ (<*>) <$> f <*> a

instance Monad AppM where
  return :: a -> AppM a
  return = pure

  -- IO (Either e a)
  -- AppM a = (IO (Either e a))

  (>>=) :: AppM a -> (a -> AppM b) -> AppM b
  (>>=) (AppM ioea) f = AppM $ ioea >>= either (pure . Left) (runAppM . f)

  -- Teacher version
  -- (>>=) appma appmatob = AppM $ do
  --    a <- runAppM appma
  --    case a of
  --      Left err -> pure $ Left err
  --      Right a  -> runAppM $ f a

instance MonadIO AppM where
  liftIO :: IO a -> AppM a
  liftIO ioa = AppM $ ioa <&> Right

instance MonadError Error AppM where
  throwError :: Error -> AppM a
  throwError e = AppM $ pure $ Left e

  catchError :: AppM a -> (Error -> AppM a) -> AppM a
  catchError (AppM ioea) f = AppM $ ioea >>= either (runAppM . f) (pure . pure)

-- This is a helper function that will `lift` an Either value into our new AppM
-- by applying `throwError` to the Left value, and using `pure` to lift the
-- Right value into the AppM.
--
-- throwError :: MonadError e m => e -> m a
-- pure :: Applicative m => a -> m a
--
liftEither :: Either Error a -> AppM a
liftEither = either throwError pure

-- Go to 'src/Level05/DB.hs' next.
