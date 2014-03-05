module System.Command.Monad
(
  Command(runCommand)
, command
) where

import Prelude
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Data.Monoid
import qualified System.Command as Cmd

newtype Command m a =
  Command { runCommand :: m (Either (Cmd.ExitCode, String) a) }

command ::
  MonadIO m
  => FilePath
  -> [String]
  -> String
  -> Command m String
command prog args stdin =
  Command $ do
    (code, stdout, stderr) <- liftIO $ Cmd.readProcessWithExitCode prog args stdin
    return $ if Cmd.isSuccess code then Right stdout else Left (code, stderr)

instance Functor m => Functor (Command m) where
  fmap f (Command m) =
    Command (fmap (either Left (Right . f)) m)

instance (Functor m, Monad m) => Applicative (Command m) where
  pure =
    Command . return . Right
  Command mf <*> Command mx =
    Command (mf >>= either (return . Left) go) where
      go f = fmap (either Left (Right . f)) mx

instance (Functor m, Monad m) => Alternative (Command m) where
  empty =
    Command (return (Left mempty))
  Command mx <|> Command my =
    Command (mx >>= either (const my) (return . Right))

instance Monad m => Monad (Command m) where
  return =
    Command . return . Right
  Command mx >>= f =
    Command (mx >>= either (return . Left) (runCommand . f))

instance Monad m => MonadPlus (Command m) where
  mzero =
    Command (return (Left mempty))
  Command mx `mplus` Command my =
    Command (mx >>= either (const my) (return . Right))

instance MonadTrans Command where
  lift =
    Command . liftM Right

instance MonadIO m => MonadIO (Command m) where
  liftIO =
    lift . liftIO
