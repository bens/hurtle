module Main where

import           Control.Applicative
import qualified Control.Concurrent        as Conc
import qualified Control.Concurrent.STM    as STM
import           Control.Monad             (when)
import qualified Data.HashMap.Strict       as HM
import           System.IO                 (stderr)
import qualified System.Log.Handler.Simple as Log
import qualified System.Log.Logger         as Log
import           Text.Read                 (readEither)

import           System.Hurtle

testConfig :: Config (STM.TVar (HM.HashMap CallId String)) String
testConfig = Config
  { configInit = STM.atomically (STM.newTVar HM.empty)
  , configTerm = \_ -> return ()
  , configSend = \hash cid msg -> do
        Log.debugM "testConfig.send" $ show (cid, msg)
        STM.atomically $ STM.modifyTVar hash (HM.insert cid msg)
  , configRecv = \hash -> do
        xM <- STM.atomically $ do
            h <- STM.readTVar hash
            case HM.keys h of
                [] -> return Nothing
                cid:_ -> do
                    STM.modifyTVar hash (HM.delete cid)
                    return $ (,) cid <$> HM.lookup cid h
        case xM of
            Nothing -> do
                Log.debugM "testConfig.receive" "No calls in progress"
                return . Fatal $ "No calls in progress"
            Just (cid, x) -> do
                Log.debugM "testConfig.receive" $ show (cid, x)
                return $ Ok cid x
  }

sendInt :: Int -> Request String String i Int
sendInt = flip makeCall readEither . show

main :: IO ()
main = do
    handler <- Log.verboseStreamHandler stderr Log.DEBUG
    let setLevel   = Log.setLevel Log.DEBUG
        setHandler = Log.setHandlers [handler]
    Log.updateGlobalLogger Log.rootLoggerName (setLevel . setHandler)
    let f x = do
            sendInt x
            when (x > 1) $
                request (pred x) >> request (pred (pred x))
            when (x == 1) $
                request 0
            return x
        doneHandler _ x = Log.infoM "main" $ "result was " ++ show x
        logHandler (LogMessage lvl section msg) = case lvl of
            Debug   -> Log.debugM section msg
            Info    -> Log.infoM section msg
            Warning -> Log.warningM section msg
            Error   -> Log.errorM section msg

    runHurtle testConfig f [5] doneHandler logHandler
    Conc.threadDelay (10^(5::Int))
    Log.infoM "main" "DONE"
