module HW3.Action
  ( HIO (..),
    HiPermission (..),
    PermissionException (..),
  )
where

import Control.Monad (ap)
import Control.Exception (Exception, throw)
import qualified Data.ByteString as Byte (readFile, writeFile)
import qualified Data.Text as Text(pack)
import Data.Set (Set, member)
import Data.Text.Encoding (decodeUtf8')
import GHC.IO.Exception (IOErrorType (PermissionDenied))
import Data.Sequence (fromList)
import HW3.Base
import System.Directory (doesFileExist, listDirectory, createDirectory, setCurrentDirectory, getCurrentDirectory)
import Control.Monad.Cont (liftM)

data HiPermission
  = AllowRead
  | AllowWrite
  deriving (Show, Eq, Ord, Enum, Bounded)

data PermissionException
  = PermissionRequired HiPermission
  deriving (Show)

instance Exception PermissionException

newtype HIO a = HIO {runHIO :: Set HiPermission -> IO a}

instance Monad HIO where
  return a = HIO (\_ -> return a)

  m >>= f = HIO (\perms -> do a <- runHIO m perms
                              runHIO (f a) perms
                                )

instance Functor HIO where
  fmap = liftM

instance Applicative HIO where
  pure = return
  (<*>) = ap

instance HiMonad HIO where
  runAction action =
    HIO
      ( \perms -> case action of
          HiActionRead filePath
            | member AllowRead perms -> do
              fileExist <- doesFileExist filePath
              if fileExist
                then do
                  bytes <- Byte.readFile filePath
                  return $
                    case decodeUtf8' bytes of
                      Right ans -> HiValueString ans
                      Left _ -> HiValueBytes bytes
                else do
                  list <- listDirectory filePath
                  return $ HiValueList $ fromList $ map (HiValueString . Text.pack) list
            | otherwise -> throw $ PermissionRequired AllowRead

          HiActionWrite filePath bytes
            | member AllowWrite perms -> do
              Byte.writeFile filePath bytes
              return HiValueNull
            | otherwise -> throw $ PermissionRequired AllowWrite

          HiActionMkDir filePath
            | member AllowWrite perms -> do
              createDirectory filePath
              return HiValueNull
            | otherwise -> throw $ PermissionRequired AllowWrite
            
          HiActionChDir filePath
            | member AllowRead perms -> do
              setCurrentDirectory filePath
              return HiValueNull
            | otherwise -> throw $ PermissionRequired AllowRead
            
            
          HiActionCwd
            | member AllowRead perms -> do
              currDir <- getCurrentDirectory
              return $ HiValueString $ Text.pack currDir
            | otherwise -> throw $ PermissionRequired AllowRead
      )
