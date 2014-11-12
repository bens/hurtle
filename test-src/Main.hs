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

testConfig :: Config String String String IO
testConfig = Config
  { configInit = STM.atomically (STM.newTVar HM.empty)
  , configTerm = \_ -> Log.debugM "testConfig.term" "finished"
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

sendInt :: Int -> Hurtle String String String i IO Int
sendInt = flip makeCall readEither . show

main :: IO ()
main = do
    handler <- Log.verboseStreamHandler stderr Log.DEBUG
    let setLevel   = Log.setLevel Log.DEBUG
        setHandler = Log.setHandlers [handler]
    Log.updateGlobalLogger Log.rootLoggerName (setLevel . setHandler)

    let doneHandler x = Log.infoM "main" $ "result was " ++ show x
        logHandler msg = case logLevel msg of
            Debug   -> Log.debugM   component (logDescription ("ERR: "++) msg)
            Info    -> Log.infoM    component (logDescription ("ERR: "++) msg)
            Warning -> Log.warningM component (logDescription ("ERR: "++) msg)
            Error   -> Log.errorM   component (logDescription ("ERR: "++) msg)
            where component = "Hurtle"

    runHurtle testConfig doneHandler logHandler [5] $ \x -> do
        _ <- sendInt x
        when (x > 1) $
            request (pred x) >> request (pred (pred x))
        when (x == 1) $
            request 0
        return x

    Conc.threadDelay (10^(5::Int))
    Log.infoM "main" "DONE"
