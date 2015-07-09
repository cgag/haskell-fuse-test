{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( runFuse
    ) where

import Control.Exception.Base (SomeException)
import Foreign.C.Error
import Data.Monoid ((<>))
import Control.Concurrent.MVar
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict as M
import System.Posix.Types
import System.Posix.Files
import System.Fuse

data DUMMY = DUMMY
debugFile = "/home/cgag/src/fuse/haskell/debug"
dbg msg = B.appendFile debugFile (msg <> "\n")

data File = File
  { stat :: !FileStat
  , contents :: !ByteString
  }


helloOpen :: FilePath
          -> OpenMode
          -> OpenFileFlags
          -> IO (Either Errno DUMMY)
helloOpen fpath mode flags = do
    return (Right DUMMY)

helloRead :: FileStore
          -> FilePath
          -> DUMMY
          -> ByteCount
          -> FileOffset
          -> IO (Either Errno ByteString)
helloRead fileStore fpath handle bc offset = do
  fileMap <- readMVar fileStore
  let (_:fname) = fpath
  dbg ("Reading " <> B.pack fname)
  case M.lookup fname fileMap of
    Just (File _ contents) -> do
      dbg ("Read " <> B.pack fname <> ", got: " <> contents)
      return (Right contents)
    Nothing -> do
      dbg ("Failed to read: " <> B.pack fname)
      return (Left eNOENT)

type FileStore = MVar (M.HashMap FilePath File)

helloReadDirectory :: FileStore
                   -> (FilePath -> IO (Either Errno [(FilePath, FileStat)]))
helloReadDirectory fileStore fpath = do
  case fpath of
    "/" -> do
      fileMap <- readMVar fileStore
      return (Right $ M.foldlWithKey' (\acc k v -> (k, stat v):acc) [] fileMap)
    _ -> return (Left eNOENT)

helloCreateDevice :: FileStore
                  -> FilePath
                  -> EntryType
                  -> FileMode
                  -> DeviceID
                  -> IO Errno
helloCreateDevice fileStore fpath entryType mode did = do
  dbg ("creating deviced with path: " <> B.pack fpath)
  ctx <- getFuseContext
  case entryType of
    RegularFile -> do
      let (_:fname) = fpath
      let newStat = (fileStat ctx){ statFileMode = mode }
      _ <- modifyMVar_ fileStore (\m -> return $ M.insert fname (File newStat "") m)
      -- (\m -> return $ files <> [(fname, (fileStat ctx) { statFileMode = mode } )])
      return eOK
    _ -> return eNOENT

-- TODO: understand and anki all these olptions
dirStat ctx = FileStat { statEntryType = Directory
                       , statFileMode = foldr1 unionFileModes
                                          [ ownerReadMode
                                          , ownerExecuteMode
                                          , ownerWriteMode
                                          , groupReadMode
                                          , groupWriteMode
                                          , groupExecuteMode
                                          , otherReadMode
                                          , otherWriteMode
                                          , otherExecuteMode
                                          ]
                       , statLinkCount = 2
                       , statFileOwner = fuseCtxUserID ctx
                       , statFileGroup = fuseCtxGroupID ctx
                       , statSpecialDeviceID = 0
                       , statFileSize = 4096
                       , statBlocks = 1
                       , statAccessTime = 0
                       , statModificationTime = 0
                       , statStatusChangeTime = 0
                       }

fileStat ctx = FileStat { statEntryType = RegularFile
                        , statFileMode = foldr1 unionFileModes
                                           [ ownerReadMode
                                           , ownerWriteMode
                                           , groupReadMode
                                           , otherReadMode
                                           ]
                        , statLinkCount = 1
                        , statFileOwner = fuseCtxUserID ctx
                        , statFileGroup = fuseCtxGroupID ctx
                        , statSpecialDeviceID = 0
                        , statFileSize = 0
                        , statBlocks = 1
                        , statAccessTime = 0
                        , statModificationTime = 0
                        , statStatusChangeTime = 0
                        }

helloOpenDirectory :: FilePath -> IO Errno
helloOpenDirectory "/" = return eOK
helloOpenDirectory _  = return eNOENT

helloGetFileSystemStats str = do
    return $ Right $ FileSystemStats
        { fsStatBlockSize = 512
        , fsStatBlockCount = 1
        , fsStatBlocksFree = 1
        , fsStatBlocksAvailable = 1
        , fsStatFileCount = 5
        , fsStatFilesFree = 10
        , fsStatMaxNameLength = 255
        }

helloGetFileStat :: FileStore -> FilePath -> IO (Either Errno FileStat)
helloGetFileStat fileStore fpath = do
  dbg ("getting file stat: " <> B.pack fpath)
  ctx <- getFuseContext
  fileMap <- readMVar fileStore
  case fpath of
    "/" -> return $ Right (dirStat ctx)
    (_:fname) -> case M.lookup fname fileMap of
                      Nothing   -> return (Left  eNOENT)
                      Just (File stat _) -> return (Right stat)

helloWrite :: FileStore
           -> FilePath
           -> DUMMY
           -> ByteString
           -> FileOffset
           -> IO (Either Errno ByteCount)
helloWrite fileStore fpath dummy bytes offset = do
  fileMap <- readMVar fileStore
  let (_:fname) = fpath
  case M.lookup fname fileMap of
    Nothing -> do
      dbg $ "Write: didn't find file (" <> B.pack fname <> ")"
      return (Left eNOENT)
    Just (File stat contents) -> do
      dbg ("Writing to " <> B.pack fname)
      let newContents = B.take (ioffset - 1) contents <> bytes
      swapMVar fileStore (M.insert fname (File (stat {statFileSize = (fromIntegral . B.length $ newContents)})newContents) fileMap)
      return $ Right (fromIntegral . B.length $ bytes)
  where
    ioffset = fromIntegral offset
    bytesWritten = (fromIntegral . B.length $ bytes)

helloSetFileTimes :: FileStore -> FilePath -> EpochTime -> EpochTime -> IO Errno
helloSetFileTimes fileStore fpath t1 t2 = return eOK

helloAccess :: FilePath -> Int -> IO Errno
helloAccess _ _ = return eOK

runFuse :: IO ()
runFuse = do
    ctx <- getFuseContext

    fileList <- newMVar (M.fromList [(".",  File {stat=dirStat ctx, contents=""})
                                    ,("..", File {stat=dirStat ctx, contents=""})
                                    ])

    fuseMain (buildOps fileList) defaultExceptionHandler
  where
    buildOps :: FileStore -> FuseOperations DUMMY
    buildOps fileStore =
        defaultFuseOps
            {
              fuseOpen               = helloOpen
            , fuseAccess             = helloAccess
            , fuseGetFileSystemStats = helloGetFileSystemStats
            , fuseOpenDirectory      = helloOpenDirectory

            , fuseRead               = helloRead fileStore
            , fuseReadDirectory      = helloReadDirectory fileStore
            , fuseGetFileStat        = helloGetFileStat fileStore
            , fuseCreateDevice       = helloCreateDevice fileStore
            , fuseWrite              = helloWrite fileStore
            , fuseRelease = (\_ _ -> return ())

            , fuseSetFileTimes       = helloSetFileTimes fileStore
            }
