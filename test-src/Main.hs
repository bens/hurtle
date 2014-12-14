{-# LANGUAGE GADTs        #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import           Control.Applicative
import qualified Control.Concurrent.STM    as STM
import           Control.Exception         (finally)
import           Control.Foldl
import           Control.Monad             (join)
import           Data.List                 (sortBy)
import qualified Data.Map                  as M
import           System.Directory          (createDirectoryIfMissing)
import           System.FilePath           ((</>))
import           System.IO                 (Handle, IOMode(..), withFile)
import qualified System.Log.Handler        as LogH
import qualified System.Log.Handler.Simple as LogH
import qualified System.Log.Logger         as Log
import qualified Test.Tasty                as Tasty
import qualified Test.Tasty.Golden         as Tasty

import           System.Hurtle

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

withLogging :: FilePath -> IO a -> IO a
withLogging path m = withFile ("test/output/" </> path) WriteMode $ \h -> do
    fileH <- LogH.streamHandler h Log.DEBUG
    let setLevel   = Log.setLevel Log.DEBUG
        setHandler = Log.setHandlers [fileH]
        noHandlers = Log.setHandlers ([] :: [LogH.GenericHandler Handle])
    Log.updateGlobalLogger Log.rootLoggerName (setLevel . setHandler)
    finally m $ do
        LogH.close fileH
        Log.updateGlobalLogger Log.rootLoggerName noHandlers

logHandler :: FoldM IO (Log (Error TestConn)) ()
logHandler = FoldM (const f) (return ()) return
  where
    f msg = case logLevel msg of
        Debug   -> Log.debugM   component (logDescription showE msg)
        Info    -> Log.infoM    component (logDescription showE msg)
        Warning -> Log.warningM component (logDescription showE msg)
        Error   -> Log.errorM   component (logDescription showE msg)
    component = "Hurtle"
    showE (ErrTest x) = "ERR: " ++ x

main :: IO ()
main = do
    createDirectoryIfMissing True "test/output"
    Tasty.defaultMain goldenTests

echoIntNew :: Int -> Hurtle s TestConn Int
echoIntNew x = request (ReqInt x)

showIntNew :: Int -> Hurtle s TestConn String
showIntNew x = request (ReqStr x)

type Test s
    =  Hurtle s TestConn Int
    -> Hurtle s TestConn [Int]
    -> Hurtle s TestConn [Int]
    -> Hurtle s TestConn (Hurtle s TestConn [Int])

goldenTests :: Tasty.TestTree
goldenTests = Tasty.testGroup "Golden"
    [ runGolden "forkAll" (forkAll 5)
    , runGolden "joinXs"  (joinXs  5)
    , runGolden "joinYs"  (joinYs  5)
    ]

runGolden :: String -> (forall s. Hurtle s TestConn [Int]) -> Tasty.TestTree
runGolden nm m =
    Tasty.goldenVsFile nm golden output . withLogging nm $ do
        x <- runHurtle InitTest logHandler m
        Log.infoM "main" $ "RESULT: " ++ show (fmap fst x)
  where
    golden = "test/golden" </> nm
    output = "test/output" </> nm

makeGolden :: Test s -> Int -> Hurtle s TestConn [Int]
makeGolden test = join . go
  where
    go 0 = fork $ pure <$> request (ReqInt 0)
    go 1 = fork $ pure <$> request (ReqInt 1)
    go n = do
        xm  <- fork $ request (ReqInt n)
        xsm <- go (n-1)
        ysm <- go (n-2)
        fmap (sortBy (flip compare)) <$> test xm xsm ysm

forkAll :: Int -> Hurtle s TestConn [Int]
forkAll = makeGolden $ \xm xsm ysm ->
    return $ do
        x  <- xm
        xs <- xsm
        ys <- ysm
        return $ x : xs ++ ys

joinXs :: Int -> Hurtle s TestConn [Int]
joinXs = makeGolden $ \xm xsm ysm -> do
    xs <- xsm
    return $ do
        x  <- xm
        ys <- ysm
        return $ x : xs ++ ys

joinYs :: Int -> Hurtle s TestConn [Int]
joinYs = makeGolden $ \xm xsm ysm -> do
    ys <- ysm
    return $ do
        x  <- xm
        xs <- xsm
        return $ x : xs ++ ys
