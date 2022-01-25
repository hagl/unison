{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Codebase.Editor.Git
  ( gitIn,
    gitTextIn,
    pullRepo,
    withIOError,
    withStatus,
    withIsolatedRepo,

    -- * Exported for testing
    gitCacheDir,

    GitBranchBehavior(..)
  )
where

import Unison.Prelude

import qualified Control.Exception
import Control.Monad.Except (MonadError, throwError)
import qualified Data.Text as Text
import Shellmet (($?), ($^), ($|))
import System.FilePath ((</>))
import Unison.Codebase.Editor.RemoteRepo (ReadRepo (..))
import qualified Unison.Codebase.GitError as GitError
import Unison.CodebasePath (CodebasePath)
import qualified Unison.Util.Exception as Ex
import UnliftIO.Directory (XdgDirectory (XdgCache), doesDirectoryExist, findExecutable, getXdgDirectory, removeDirectoryRecursive)
import UnliftIO.IO (hFlush, stdout)
import qualified Data.ByteString.Base16 as ByteString
import qualified Data.Char as Char
import Unison.Codebase.GitError (GitProtocolError)
import UnliftIO (handleIO, MonadUnliftIO)
import qualified UnliftIO

-- Name of the main branch used in codebase repositories
mainBranchRef :: Text
mainBranchRef = "main"


-- https://superuser.com/questions/358855/what-characters-are-safe-in-cross-platform-file-names-for-linux-windows-and-os
encodeFileName :: String -> FilePath
encodeFileName = let
  go ('.' : rem) = "$dot$" <> go rem
  go ('$' : rem) = "$$" <> go rem
  go (c : rem) | elem @[] c "/\\:*?\"<>|" || not (Char.isPrint c && Char.isAscii c)
                 = "$x" <> encodeHex [c] <> "$" <> go rem
               | otherwise = c : go rem
  go [] = []
  encodeHex :: String -> String
  encodeHex = Text.unpack . Text.toUpper . ByteString.encodeBase16 .
              encodeUtf8 . Text.pack
  in go

gitCacheDir :: MonadIO m => Text -> m FilePath
gitCacheDir url =
  getXdgDirectory XdgCache
    $   "unisonlanguage"
    </> "gitfiles"
    </> encodeFileName (Text.unpack url)

withStatus :: MonadIO m => String -> m a -> m a
withStatus str ma = do
  flushStr str
  a <- ma
  flushStr (const ' ' <$> str)
  pure a
  where
  flushStr str = do
    liftIO . putStr $ "  " ++ str ++ "\r"
    hFlush stdout

-- | Run an action on an isolated copy of the provided repo. The repo is deleted when the
-- action exits or fails.
withIsolatedRepo ::
  forall m r.
  (MonadUnliftIO m) =>
  FilePath ->
  (FilePath -> m r) ->
  m (Either GitProtocolError r)
withIsolatedRepo srcPath action = do
  UnliftIO.withSystemTempDirectory "ucm-isolated-repo" $ \tempDir -> do
    copyCommand tempDir >>= \case
      Left gitErr -> pure $ Left (GitError.CopyException srcPath tempDir (show gitErr))
      Right () -> Right <$> action tempDir
  where
    copyCommand :: FilePath -> m (Either IOException ())
    copyCommand dest = UnliftIO.tryIO . liftIO $
      "git" $^ (["clone", "--quiet"] ++ ["file://" <> Text.pack srcPath, Text.pack dest])

data GitBranchBehavior =
      CreateBranchIfMissing
    | RequireExistingBranch

-- | Given a remote git repo url, and branch/commit hash (currently
-- not allowed): checks for git, clones or updates a cached copy of the repo
pullRepo :: forall m. (MonadIO m, MonadError GitProtocolError m) => ReadRepo -> GitBranchBehavior -> m CodebasePath
pullRepo repo@(ReadGitRepo {url=uri, commitish}) branchBehavior = do
  checkForGit
  localPath <- gitCacheDir uri

  (pullBranch, createBranch) <- do
    case commitish of
      Nothing -> pure (Nothing, Nothing)
      Just ref -> do
        exists <- liftIO $ doesRemoteBranchExist ref
        case branchBehavior of
          CreateBranchIfMissing
            | exists -> pure (Just ref, Nothing)
            | otherwise -> pure (Nothing, Just ref)
          RequireExistingBranch
            | exists -> pure (Just ref, Nothing)
            | otherwise -> throwError (GitError.RemoteRefNotFound ref)

  ifM (doesDirectoryExist localPath)
    -- try to update existing directory
    (ifM (isGitRepo localPath)
      (checkoutExisting localPath pullBranch)
      (throwError (GitError.UnrecognizableCacheDir repo localPath)))
    -- directory doesn't exist, so clone anew
    (checkOutNew localPath pullBranch)
  case createBranch of
    Nothing -> pure ()
    Just b -> liftIO ("git" $^ ["checkout", "--branch", b, "--track"])
  pure localPath
  where
  doesRemoteBranchExist :: Text -> IO Bool
  doesRemoteBranchExist branchName = do
    output <- "git" $| ["ls-remote", "--heads", uri, branchName]
    pure . not . Text.null . Text.strip $ output

  -- | Do a `git clone` (for a not-previously-cached repo).
  checkOutNew :: CodebasePath -> Maybe Text -> m ()
  checkOutNew localPath commitish = do
    withStatus ("Downloading from " ++ Text.unpack uri ++ " ...") $
      (liftIO $
        "git" $^ (["clone", "--quiet"] ++ ["--depth", "1"]
         ++ maybe [] (\t -> ["--branch", t]) commitish
         ++ [uri, Text.pack localPath]))
        `withIOError` (throwError . GitError.CloneException repo . show)
    isGitDir <- liftIO $ isGitRepo localPath
    unless isGitDir . throwError $ GitError.UnrecognizableCheckoutDir repo localPath

  -- | Do a `git pull` on a cached repo.
  checkoutExisting :: (MonadIO m, MonadError GitProtocolError m)
                   => FilePath
                   -> Maybe Text
                   -> m ()
  checkoutExisting localPath maybeRemoteRef =
    ifM (isEmptyGitRepo localPath)
      -- I don't know how to properly update from an empty remote repo.
      -- As a heuristic, if this cached copy is empty, then the remote might
      -- be too, so this impl. just wipes the cached copy and starts from scratch.
      (goFromScratch maybeRemoteRef)
      -- Otherwise proceed!
      do
        succeeded <- liftIO . handleIO (const $ pure False) $ do
                         withStatus ("Updating cached copy of " ++ Text.unpack uri ++ " ...") $ do
                          -- Fetch only the latest commit, we don't need history.
                          gitIn localPath (["fetch", "origin", remoteRef, "--quiet"] ++ ["--depth", "1"])
                          fetchHeadHash <- gitTextIn localPath ["rev-parse", "FETCH_HEAD"]
                          gitIn localPath ["checkout", remoteRef]
                          -- If the local checkout fails it means we don't have a local branch for this ref yet,
                          -- create a new branch from main to use.
                            $? gitIn localPath ["checkout", "--branch", remoteRef, mainBranchRef]
                          headHash <- gitTextIn localPath ["rev-parse", "HEAD"]

                          -- Only do a hard reset if the remote has actually changed.
                          -- This allows us to persist any codebase migrations in the dirty work tree,
                          -- and avoid re-migrating a codebase we've migrated before.
                          when (fetchHeadHash /= headHash) do
                            -- Reset our branch to point at the latest code from the remote.
                            gitIn localPath ["reset", "--hard", "--quiet", "FETCH_HEAD"]
                            -- Wipe out any unwanted files which might be sitting around, but aren't in the commit.
                            -- Note that this wipes out any in-progress work which other ucm processes may
                            -- have in progress, which we may want to handle more nicely in the future.
                            gitIn localPath ["clean", "-d", "--force", "--quiet"]
                          pure True
        when (not succeeded) $ goFromScratch maybeRemoteRef

    where
      remoteRef :: Text
      remoteRef = fromMaybe mainBranchRef maybeRemoteRef
      goFromScratch :: (MonadIO m, MonadError GitProtocolError m) => Maybe Text -> m  ()
      goFromScratch ref = do
        wipeDir localPath
        checkOutNew localPath ref

  isEmptyGitRepo :: MonadIO m => FilePath -> m Bool
  isEmptyGitRepo localPath = liftIO $
    -- if rev-parse succeeds, the repo is _not_ empty, so return False; else True
    (gitTextIn localPath ["rev-parse", "--verify", "--quiet", "HEAD"] $> False)
      $? pure True

  -- | try removing a cached copy
  wipeDir localPath = do
    e <- Ex.tryAny . whenM (doesDirectoryExist localPath) $
      removeDirectoryRecursive localPath
    case e of
      Left e -> throwError (GitError.CleanupError e)
      Right _ -> pure ()

-- | See if `git` is on the system path.
checkForGit :: MonadIO m => MonadError GitProtocolError m => m ()
checkForGit = do
  gitPath <- liftIO $ findExecutable "git"
  when (isNothing gitPath) $ throwError GitError.NoGit

-- | Does `git` recognize this directory as being managed by git?
isGitRepo :: MonadIO m => FilePath -> m Bool
isGitRepo dir = liftIO $
  (True <$ gitIn dir ["rev-parse"]) $? pure False

-- | Perform an IO action, passing any IO exception to `handler`
withIOError :: MonadIO m => IO a -> (IOException -> m a) -> m a
withIOError action handler =
  liftIO (fmap Right action `Control.Exception.catch` (pure . Left)) >>=
    either handler pure

-- | Generate some `git` flags for operating on some arbitary checked out copy
setupGitDir :: FilePath -> [Text]
setupGitDir localPath =
  ["--git-dir", Text.pack $ localPath </> ".git"
  ,"--work-tree", Text.pack localPath]

gitIn :: MonadIO m => FilePath -> [Text] -> m ()
gitIn localPath args = liftIO $ "git" $^ (setupGitDir localPath <> args)

gitTextIn :: MonadIO m => FilePath -> [Text] -> m Text
gitTextIn localPath args = liftIO $ "git" $| setupGitDir localPath <> args
