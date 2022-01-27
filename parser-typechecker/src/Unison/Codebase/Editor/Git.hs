{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Codebase.Editor.Git
  ( gitIn,
    gitTextIn,
    withRepo,
    withIOError,
    withStatus,
    withIsolatedRepo,
    mainBranchRef,
    checkoutAndPullRef,

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
-- Note that you should probably explicitly check out whatever ref you want in the isolated
-- repo, since the repo it copied could be in some unknown state.
withIsolatedRepo ::
  forall m r.
  (MonadUnliftIO m) =>
  FilePath ->
  (FilePath -> m r) ->
  m (Either GitProtocolError r)
withIsolatedRepo srcPath action = do
    -- let tempDir = "/Users/cpenner/Desktop/temp/unison-staging"
  UnliftIO.withSystemTempDirectory "ucm-isolated-repo" $ \tempDir -> do
    copyCommand tempDir >>= \case
      Left gitErr -> pure $ Left (GitError.CopyException srcPath tempDir (show gitErr))
      Right () -> Right <$> action tempDir
  where
    copyCommand :: FilePath -> m (Either IOException ())
    copyCommand dest = UnliftIO.tryIO . liftIO $
      "git" $^ (["clone", "--quiet"]
                ++ ["file://" <> Text.pack srcPath, Text.pack dest]
               )

data GitBranchBehavior =
      CreateBranchIfMissing
    | RequireExistingBranch

withRepo ::
  forall m a.
  (MonadUnliftIO m) =>
  ReadRepo ->
  GitBranchBehavior ->
  (CodebasePath -> m a) ->
  m (Either GitProtocolError a)
withRepo repo@(ReadGitRepo {url = uri, branch = mayGitBranch}) branchBehavior action = UnliftIO.try $ do
  throwExceptT $ checkForGit
  gitCachePath <- gitCacheDir uri
  -- Ensure we have the main branch in the cache dir no matter what
  throwExceptT $ cloneIfMissing repo {branch = Nothing} gitCachePath
  case mayGitBranch of
    Nothing -> do
      throwExceptT $ checkoutAndPullRef gitCachePath repo
      action gitCachePath
    Just gitRef ->
      throwEitherM . withIsolatedRepo gitCachePath $ \workDir -> do
        doesRemoteRefExist gitRef >>= \case
          True -> do
            fetchHead <- shallowFetch workDir uri gitRef
            gitIn workDir ["checkout", "-B", gitRef, fetchHead]
            action workDir
          False ->
            case branchBehavior of
              RequireExistingBranch -> UnliftIO.throwIO (GitError.RemoteRefNotFound uri gitRef)
              CreateBranchIfMissing -> do
                ignoreFailure $ gitIn workDir ["branch", "-D", gitRef]
                gitIn workDir ["checkout", "--orphan", gitRef]
                gitIn workDir ["rm", "--ignore-unmatch", "-rf", "."]
                action workDir
  where
    ignoreFailure :: forall m. MonadIO m => IO () -> m ()
    ignoreFailure cmd = liftIO (cmd $? pure ())
    doesRemoteRefExist :: Text -> m Bool
    doesRemoteRefExist branchName = liftIO $ do
      output <- "git" $| ["ls-remote", "--heads", uri, branchName]
      pure . not . Text.null . Text.strip $ output

-- withWorkDir :: FilePath -> m a
-- withWorkDir workDir = do
--   (pullBranch, createBranch) <- do
--     exists <- doesRemoteBranchExist gitBranch
--     case branchBehavior of
--       CreateBranchIfMissing
--         | exists -> pure (Just gitBranch, Nothing)
--         | otherwise -> pure (Nothing, Just gitBranch)
--       RequireExistingBranch
--         | exists -> pure (Just gitBranch, Nothing)
--         | otherwise -> throwError (GitError.RemoteRefNotFound uri gitBranch)

    --   case createBranch of
    --     Nothing -> checkoutAndPullRef localPath repo{branch=pullBranch}
    --     Just b -> do
    --       -- We've already established this branch does not exist on the remote,
    --       -- delete any stale local branch we may have.
    --       -- It's possible we've got the branch we're working with currently checked out,
    --       -- so we need to check out some other branch before deleting it.
    --       ignoreFailure (gitIn localPath ["checkout", "-B", "_unison_temp_branch"])
    --       ignoreFailure (gitIn localPath ["branch", "-D", b])
    --       -- Create an empty local branch.
    --       gitIn localPath ["checkout", "--orphan", b]
    --       ignoreFailure (gitIn localPath ["rm", "--ignore-unmatch", "-rf", "."])
    --   pure localPath

shallowFetch :: MonadIO m => FilePath -> Text -> Text -> m Text
shallowFetch localPath uri remoteRef = do
  gitIn localPath (["fetch", uri, remoteRef, "--quiet"] ++ ["--depth", "1"])
  fetchHeadHash <- gitTextIn localPath ["rev-parse", "FETCH_HEAD"]
  pure fetchHeadHash

checkoutAndPullRef ::
  forall m.
  (MonadIO m, MonadError GitProtocolError m) =>
  FilePath ->
  ReadRepo ->
  m ()
checkoutAndPullRef localPath repo@(ReadGitRepo{url=uri, branch=mayRemoteRef}) =
  ifM (isEmptyGitRepo localPath)
    -- I don't know how to properly update from an empty remote repo.
    -- As a heuristic, if this cached copy is empty, then the remote might
    -- be too, so this impl. just wipes the cached copy and starts from scratch.
    goFromScratch
    -- Otherwise proceed!
    do
      succeeded <- liftIO . handleIO (const $ pure False) $ do
                       withStatus ("Updating cached copy of " ++ Text.unpack uri ++ " ...") $ do
                        -- Fetch only the latest commit, we don't need history.
                        gitIn localPath (["fetch", uri, remoteRef, "--quiet"] ++ ["--depth", "1"])
                        fetchHeadHash <- gitTextIn localPath ["rev-parse", "FETCH_HEAD"]
                        gitIn localPath ["checkout", remoteRef]
                        -- If the local checkout fails it means we don't have a local branch for this ref yet,
                        -- create a new branch from main to use.
                          $? gitIn localPath ["checkout", "-b", remoteRef, mainBranchRef]
                        headHash <- gitTextIn localPath ["rev-parse", remoteRef]

                        -- Only do a hard reset if the remote has actually changed.
                        -- This allows us to persist any codebase migrations in the dirty work tree,
                        -- and avoid re-migrating a codebase we've migrated before.
                        when (fetchHeadHash /= headHash) do
                          -- Reset our branch to point at the latest code from the remote.
                          gitIn localPath ["reset", "--hard", "--quiet", fetchHeadHash]
                          -- Wipe out any unwanted files which might be sitting around, but aren't in the commit.
                          -- Note that this wipes out any in-progress work which other ucm processes may
                          -- have in progress, which we may want to handle more nicely in the future.
                          gitIn localPath ["clean", "-d", "--force", "--quiet"]
                        pure True
      when (not succeeded) $ goFromScratch
  where
    remoteRef :: Text
    remoteRef = fromMaybe mainBranchRef mayRemoteRef

    goFromScratch :: (MonadIO m, MonadError GitProtocolError m) => m  ()
    goFromScratch = do
      traceM "Something went wrong, going from scratch!"
      wipeDir localPath
      cloneIfMissing repo localPath

    isEmptyGitRepo :: MonadIO m => FilePath -> m Bool
    isEmptyGitRepo localPath = liftIO $
      -- if rev-parse succeeds, the repo is _not_ empty, so return False; else True
      (gitTextIn localPath ["rev-parse", "--verify", "--quiet", "HEAD"] $> False)
        $? pure True

    -- | try removing a cached copy
    wipeDir :: FilePath -> m ()
    wipeDir localPath = do
      e <- Ex.tryAny . whenM (doesDirectoryExist localPath) $
        removeDirectoryRecursive localPath
      case e of
        Left e -> throwError (GitError.CleanupError e)
        Right _ -> pure ()


-- | Do a `git clone` (for a not-previously-cached repo).
cloneIfMissing :: (MonadIO m, MonadError GitProtocolError m) => ReadRepo -> CodebasePath -> m ()
cloneIfMissing repo@(ReadGitRepo {url=uri, branch=gitBranch}) localPath = do
  doesDirectoryExist localPath >>= \case
    True ->
      whenM (not <$> isGitRepo localPath) $ do
        throwError (GitError.UnrecognizableCacheDir repo localPath)
    False -> do
      -- directory doesn't exist, so clone anew
      cloneRepo
  where
    cloneRepo = do
      withStatus ("Downloading from " ++ Text.unpack uri ++ " ...") $
        (liftIO $
          "git" $^ (["clone", "--quiet"] ++ ["--depth", "1"]
           ++ maybe [] (\t -> ["--branch", t]) gitBranch
           ++ [uri, Text.pack localPath]))
          `withIOError` (throwError . GitError.CloneException repo . show)
      isGitDir <- liftIO $ isGitRepo localPath
      unless isGitDir . throwError $ GitError.UnrecognizableCheckoutDir repo localPath

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
gitIn localPath args = do
  traceShowM (localPath, args)
  liftIO $ "git" $^ (setupGitDir localPath <> args)

gitTextIn :: MonadIO m => FilePath -> [Text] -> m Text
gitTextIn localPath args = do
  traceShowM (localPath, args)
  liftIO $ "git" $| setupGitDir localPath <> args
