module SubProcess (handleOutput, SubProcess (..)) where

import qualified Control.Concurrent as Concurrent
import qualified Control.Monad as Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import qualified System.IO as IO
import qualified System.Process as Proc

data SubProcess = SubProcess
  { pHProc :: Proc.ProcessHandle,
    pHOut :: IO.Handle,
    pHErr :: IO.Handle,
    pOnOut :: String -> IO (),
    pOnErr :: String -> IO ()
  }

data Buffer = Buffer {bOut :: String, bErr :: String}

handleOutput :: SubProcess -> IO ()
handleOutput sub = handleOutput' sub Buffer {bOut = "", bErr = ""}

handleOutput' :: SubProcess -> Buffer -> IO ()
handleOutput' sub buf = do
  (bOut', out) <- getOutput (bOut buf) (pHOut sub)
  mapM_ (pOnOut sub) out
  (bErr', err) <- getOutput (bErr buf) (pHErr sub)
  mapM_ (pOnErr sub) err
  status <- Proc.getProcessExitCode (pHProc sub)
  case status of
    Nothing -> handleOutput' sub Buffer {bOut = bOut', bErr = bErr'}
    Just _ -> getLastOutput sub

getLastOutput :: SubProcess -> IO ()
getLastOutput sub = do
  out <- contents (pHOut sub)
  mapM_ (pOnOut sub) out
  err <- contents (pHErr sub)
  mapM_ (pOnErr sub) err
  where
    contents h = do
      bs <- BS.hGetContents h
      pure . lines . filter (/= '\r') . UTF8.toString $ bs

getOutput :: String -> IO.Handle -> IO (String, [String])
getOutput buf hOut = do
  bs <- BS.hGetNonBlocking hOut 256
  Monad.when (BS.null bs) $ Concurrent.threadDelay 250
  pure $ getLines (buf ++ (filter (/= '\r') . UTF8.toString $ bs))

getLines :: String -> (String, [String])
getLines "" = ("", [])
getLines buf =
  if (== '\n') . last $ buf
    then ("", ls)
    else (last ls, init ls)
  where
    ls = lines buf
