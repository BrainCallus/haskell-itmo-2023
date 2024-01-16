{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia    #-}
{-# LANGUAGE InstanceSigs   #-}
{-# LANGUAGE LambdaCase     #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module HW5.Action
  ( HiPermission(..)
  , PermissionException(..)
  , HIO(..)
  ) where

import           Control.Exception      (Exception)
import           Control.Exception.Base (throw)
import           Control.Monad.Reader
import qualified Data.ByteString        as BString
import           Data.Sequence          (fromList)
import           Data.Set               (Set, member)
import qualified Data.Text              as Txt
import           Data.Text.Encoding     (decodeUtf8')
import qualified Data.Text.IO           as TIO
import           Data.Time              (getCurrentTime)
import           HW5.Base
import           System.Directory
import           System.Random          (getStdRandom, uniformR)

data HiPermission
  = AllowRead
  | AllowWrite
  | AllowTime
  deriving (Show, Eq, Ord, Bounded, Enum)

newtype PermissionException =
  PermissionRequired HiPermission
  deriving (Show)

instance Exception PermissionException

newtype HIO a = HIO
  { runHIO :: Set HiPermission -> IO a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadReader (Set HiPermission)
             ) via (ReaderT (Set HiPermission) IO)

instance HiMonad HIO where
  runAction :: HiAction -> HIO HiValue
  runAction =
    \case
      HiActionRead f ->
        doWithPermission
          (do
             isFile <- liftIO (doesFileExist f)
             if isFile
               then do
                 file <- liftIO $ BString.readFile f
                 case decodeUtf8' file of
                   Left _        -> return (HiValueBytes file)
                   Right content -> return (HiValueString content)
               else do
                 dirs <- liftIO (listDirectory f)
                 return
                   (HiValueList (fromList (HiValueString . Txt.pack <$> dirs))))
          AllowRead
      HiActionWrite f content ->
        doWithPermission
          (do
             liftIO (BString.writeFile f content)
             return HiValueNull)
          AllowWrite
      HiActionMkDir dir ->
        doWithPermission
          (do
             liftIO (createDirectory dir)
             return HiValueNull)
          AllowWrite
      HiActionChDir dir ->
        doWithPermission
          (do
             liftIO (setCurrentDirectory dir)
             return HiValueNull)
          AllowRead
      HiActionCwd ->
        doWithPermission
          (do
             dir <- liftIO getCurrentDirectory
             return (HiValueString (Txt.pack dir)))
          AllowRead
      HiActionNow ->
        doWithPermission
          (do
             time <- liftIO getCurrentTime
             return (HiValueTime time))
          AllowTime
      HiActionRand left right -> do
        rand <- getStdRandom (uniformR (left, right))
        return (HiValueNumber (toRational rand))
      HiActionEcho text ->
        doWithPermission
          (do
             liftIO (TIO.putStrLn text)
             return HiValueNull)
          AllowWrite

doWithPermission :: HIO HiValue -> HiPermission -> HIO HiValue
doWithPermission hio permType = do
  permissions <- ask
  if member permType permissions
    then hio
    else throw (PermissionRequired permType)
