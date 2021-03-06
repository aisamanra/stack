{-# LANGUAGE OverloadedStrings #-}
-- | Tag a Binary instance with the stack version number to ensure we're
-- reading a compatible format.
module Data.Binary.VersionTagged
    ( taggedDecodeOrLoad
    , taggedEncodeFile
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Binary (Binary (..), encodeFile, decodeFileOrFail, putWord8, getWord8)
import Control.Exception.Enclosed (tryIO)
import qualified Paths_stack
import Stack.Types.Version (Version, fromCabalVersion)
import System.FilePath (takeDirectory)
import System.Directory (createDirectoryIfMissing)
import qualified Data.ByteString as S
import Data.ByteString (ByteString)
import Control.Monad (forM_, when)

tag :: Version
tag = fromCabalVersion Paths_stack.version

magic :: ByteString
magic = "STACK"

newtype WithTag a = WithTag a
instance Binary a => Binary (WithTag a) where
    get = do
        forM_ (S.unpack magic) $ \w -> do
            w' <- getWord8
            when (w /= w')
                $ fail "Mismatched magic string, forcing a recompute"
        tag' <- get
        if tag == tag'
            then fmap WithTag get
            else fail "Mismatched tags, forcing a recompute"
    put (WithTag x) = do
        mapM_ putWord8 $ S.unpack magic
        put tag
        put x

-- | Write to the given file, with a version tag.
taggedEncodeFile :: (Binary a, MonadIO m)
                 => FilePath
                 -> a
                 -> m ()
taggedEncodeFile fp x = liftIO $ do
    createDirectoryIfMissing True $ takeDirectory fp
    encodeFile fp $ WithTag x

-- | Read from the given file. If the read fails, run the given action and
-- write that back to the file. Always starts the file off with the version
-- tag.
taggedDecodeOrLoad :: (Binary a, MonadIO m)
                   => FilePath
                   -> m a
                   -> m a
taggedDecodeOrLoad fp mx = do
    eres <- liftIO $ tryIO $ decodeFileOrFail fp
    case eres of
        Right (Right (WithTag x)) -> return x
        _ -> do
            x <- mx
            taggedEncodeFile fp x
            return x
