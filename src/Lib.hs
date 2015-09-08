{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( runFuse
    ) where

import           Control.Concurrent.MVar
import           Control.Exception.Base  (SomeException)
import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Char8   as B
import qualified Data.HashMap.Strict     as M
import           Data.Monoid             ((<>))
import           Foreign.C.Error
import           System.Fuse
import           System.Posix.Files
import           System.Posix.Types
import Data.List (intercalate)
import Data.List.Split (splitOn)

data DUMMY = DUMMY
debugFile = "/home/cgag/src/fuse/haskell/debug"
dbg msg = B.appendFile debugFile (msg <> "\n")


data Contents = Dir  { d_contents :: M.HashMap FilePath Entry }
              | File { f_contents :: !ByteString }
              deriving Show

ata Entry = Entry
    { stat :: FileStat
    , contents :: Contents
    } deriving Show


helloOpen :: FilePath
          -> OpenMode
          -> OpenFileFlags
          -> IO (Either Errno DUMMY)
helloOpen fpath mode flags = return (Right DUMMY)

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
        Just (Entry _ (File contents)) -> do
            dbg ("Read " <> B.pack fname <> ", got: " <> contents)
            return (Right contents)
        Just (Entry _ (Dir contents)) -> do
            dbg ("Read dir " <> B.pack fname)
            return (Right "DIRCONTENTS")
        Nothing -> do
            dbg ("Failed to read: " <> B.pack fname)
            return (Left eNOENT)

type FileStore = MVar (M.HashMap FilePath Entry)

helloReadDirectory :: FileStore
                   -> FilePath -> IO (Either Errno [(FilePath, FileStat)])
helloReadDirectory fileStore fpath =
    case fpath of
        "/" -> do
            fileMap <- readMVar fileStore
            return (Right (fileList fileMap))
        _ -> return (Left eNOENT)
  where
    fileList = M.foldlWithKey' (\acc k v -> (k, stat v):acc) []

helloCreateDevice :: FileStore
                  -> FilePath
                  -> EntryType
                  -> FileMode
                  -> DeviceID
                  -> IO Errno
helloCreateDevice fileStore fpath entryType mode did = do
    dbg ("creating device with path: " <> B.pack fpath)
    ctx <- getFuseContext
    fileMap <- readMVar fileStore
    case entryType of
        RegularFile -> do
            let (fname:restOfPath) = filter (/= "") . splitOn "/" $ fpath
            let newStat = (fileStat ctx) { statFileMode = mode }
            _ <- swapMVar fileStore (M.insert fname (Entry newStat (File "")) fileMap)
            return eOK
        _ -> do
          dbg ("Failed to create unknown device type with path: " <> B.pack fpath)
          return eNOENT

helloCreateDirectory :: FileStore -> FilePath -> FileMode -> IO Errno
helloCreateDirectory fileStore fpath mode = do
    dbg ("creating directory with path: " <> B.pack fpath)
    ctx <- getFuseContext
    fileMap <- readMVar fileStore
    let (_:fname) = fpath
    let newStat = (dirStat ctx) { statFileMode=mode }
    swapMVar fileStore
             (M.insert fname
                 (Entry newStat
                     (Dir (M.fromList [(".",  Entry { stat=dirStat ctx
                                                    , contents=(Dir M.empty)})
                                      ,("..", Entry { stat=dirStat ctx
                                                    , contents=(Dir M.empty)})
                                      ])))
                 fileMap)
    return eOK


helloOpenDirectory :: FilePath -> IO Errno
helloOpenDirectory "/" = return eOK
helloOpenDirectory _   = return eNOENT

helloGetFileSystemStats :: String -> IO (Either Errno FileSystemStats)
helloGetFileSystemStats str =
    return $ Right FileSystemStats
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
        _ -> do
            let (fname:restOfPath) = filter (/= "") . splitOn "/" $ fpath
            dbg ("looking up fname of: " <> B.pack fname)
            case M.lookup fname fileMap of
                Nothing -> do
                    dbg ("Failed to find " <> B.pack fname)
                    dbg ("Used map" <> B.pack (show fileMap))
                    return (Left eNOENT)
                Just (Entry stat (File _)) -> do
                    dbg ("Found file")
                    return (Right stat)
                Just (Entry stat (Dir _)) -> do
                    dbg "Found dir"
                    error "need a function that recursively looks up stats"

helloWrite :: FileStore
           -> FilePath
           -> DUMMY
           -> ByteString
           -> FileOffset
           -> IO (Either Errno ByteCount)
helloWrite fileStore fpath _ bytes offset = do
    dbg ("writing file: " <> B.pack fpath)
    fileMap <- readMVar fileStore
    let (_:fname) = fpath
    case M.lookup fname fileMap of
        Nothing -> do
          dbg $ "Write: didn't find file (" <> B.pack fname <> ")"
          return (Left eNOENT)
        Just (Entry stat contents) -> do
            case contents of
                File fcontents -> do
                    dbg ("Writing to file: -- " <> B.pack fname)
                    writeFile fileMap stat fname fcontents
                Dir dcontents -> do
                    dbg "Writing to DIR, what"
                    return (Left eNOENT)
  where
    writeFile fileMap stat fname contents = do
        let newContents = B.take (ioffset - 1) contents <> bytes
        swapMVar fileStore
                 (M.insert fname
                           (Entry
                              (stat {
                                  statFileSize = fromIntegral . B.length $ newContents
                              })
                              (File newContents))
                          fileMap)
        return $ Right (fromIntegral . B.length $ bytes)
    ioffset = fromIntegral offset
    bytesWritten = fromIntegral (B.length bytes)

helloSetFileTimes :: FileStore -> FilePath -> EpochTime -> EpochTime -> IO Errno
helloSetFileTimes fileStore fpath t1 t2 = return eOK

helloAccess :: FilePath -> Int -> IO Errno
helloAccess _ _ = return eOK

helloRemoveLink :: FileStore -> FilePath -> IO Errno
helloRemoveLink fileStore fpath = do
    fileMap <- readMVar fileStore
    let (_:fname) = fpath
    case M.lookup fname fileMap of
        Nothing -> return eNOENT
        Just _ -> do
            swapMVar fileStore (M.delete fname fileMap)
            return eOK



-- TODO: understand and anki all these olptions
dirStat ctx = FileStat { statEntryType = Directory
                       , statFileMode = foldr1 unionFileModes
                                          [ ownerReadMode
                                          , ownerExecuteMode
                                          , ownerWriteMode
                                          , groupReadMode
                                          , groupExecuteMode
                                          , otherReadMode
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

runFuse :: IO ()
runFuse = do
    ctx <- getFuseContext

    fileList <- newMVar (M.fromList [(".",  Entry {stat=dirStat ctx, contents=(Dir M.empty)})
                                    ,("..", Entry {stat=dirStat ctx, contents=(Dir M.empty)})
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
            , fuseRelease            = \_ _ -> return ()
            , fuseRemoveLink         = helloRemoveLink fileStore
            , fuseCreateDirectory    = helloCreateDirectory fileStore

            , fuseSetFileTimes       = helloSetFileTimes fileStore
            }
