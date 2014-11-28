{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import           Control.Applicative
import qualified Control.Concurrent        as Conc
import qualified Control.Concurrent.STM    as STM
import           Control.Monad             (when)
import qualified Data.Map                  as M
import           System.IO                 (stderr)
import qualified System.Log.Handler.Simple as Log
import qualified System.Log.Logger         as Log

import           System.Hurtle             as Old

data TestConn i
    = TestConn (STM.TVar (M.Map (WrapOrdF i Int) Int))
               (STM.TVar (M.Map (WrapOrdF i String) Int))

instance Connection TestConn where
    data InitArgs TestConn = InitTest
    data Request TestConn a where
        ReqInt :: Int -> Request TestConn Int
        ReqStr :: Int -> Request TestConn String
    data Error TestConn = ErrTest String deriving Show
    type M TestConn = IO

    initialise InitTest = do
        Log.debugM "testConfig.initialise" "Initialising"
        TestConn <$> STM.atomically (STM.newTVar M.empty)
                 <*> STM.atomically (STM.newTVar M.empty)

    finalise (TestConn _ _) = Log.debugM "testConfig.finalise" "Finished"

    send (TestConn intVar _) cid (ReqInt i) =
        STM.atomically $ STM.modifyTVar intVar (M.insert (WrapOrdF cid) i)
    send (TestConn _ strVar) cid (ReqStr i) =
        STM.atomically $ STM.modifyTVar strVar (M.insert (WrapOrdF cid) i)

    receive (TestConn intVar strVar) = do
        let getInt = do
                h <- STM.readTVar intVar
                case M.keys h of
                    [] -> STM.retry
                    cid:_ -> do
                        STM.modifyTVar intVar (M.delete cid)
                        return $ (,) cid (h M.! cid)
            getStr = do
                h <- STM.readTVar strVar
                case M.keys h of
                    [] -> STM.retry
                    cid:_ -> do
                        STM.modifyTVar strVar (M.delete cid)
                        return $ (,) cid (h M.! cid)

        xM <- STM.atomically $ (Left <$> getInt) <|> (Right <$> getStr)
        case xM of
            Left  (WrapOrdF cid, x) -> return $ Ok cid x
            Right (WrapOrdF cid, x) -> return $ Ok cid (show x)

main :: IO ()
main = do
    mainOld

echoIntOld :: Int -> Old.Hurtle TestConn i r Int
echoIntOld x = Old.makeCall (ReqInt x)

showIntOld :: Int -> Old.Hurtle TestConn i r String
showIntOld x = Old.makeCall (ReqStr x)

mainOld :: IO ()
mainOld = do
    handler <- Log.verboseStreamHandler stderr Log.DEBUG
    let setLevel   = Log.setLevel Log.DEBUG
        setHandler = Log.setHandlers [handler]
    Log.updateGlobalLogger Log.rootLoggerName (setLevel . setHandler)

    let doneHandler x = Log.infoM "main" $ "result was " ++ show x
        errHandler (ErrTest msg) = "ERR: " ++ msg
        logHandler msg = case logLevel msg of
            Debug   -> Log.debugM   component (logDescription errHandler msg)
            Info    -> Log.infoM    component (logDescription errHandler msg)
            Warning -> Log.warningM component (logDescription errHandler msg)
            Error   -> Log.errorM   component (logDescription errHandler msg)
            where component = "Hurtle"

    Old.runHurtle InitTest doneHandler logHandler [5] $ \x -> do
        _ <- echoIntOld x
        _ <- showIntOld x
        when (x > 1) $
            Old.request (pred x) >> Old.request (pred (pred x))
        when (x == 1) $
            Old.request 0
        return x

    Conc.threadDelay (10^(5::Int))
    Log.infoM "main" "DONE"
