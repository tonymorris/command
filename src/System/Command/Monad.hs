module System.Command.Monad
(
  Command(runCommand)
, command
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
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

instance Applicative m => Applicative (Command m) where
  pure =
    Command . pure . Right
  Command mf <*> Command mx =
    Command (ap <$> mf <*> mx)

instance Monad m => Monad (Command m) where
  return =
    Command . return . Right
  Command mx >>= f =
    Command (mx >>= either (return . Left) (runCommand . f))

instance MonadTrans Command where
  lift =
    Command . liftM Right

instance MonadIO m => MonadIO (Command m) where
  liftIO =
    lift . liftIO
