{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
module PIO where

-- (runPIO, runWorkStealingPIOs, WSParams(..))

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Control.Monad.State.Strict
import Data.IORef
import Data.List
import Data.Typeable
import Debug.Trace
import qualified Data.IntMap.Strict as DIM
import System.IO
import Text.Printf(printf)

-- Bounded parallel ordered IO.
--
-- This library allows one to define `IO` tasks (`PIO` monad), which can
-- run in parallel.  Such tasks can issue both immediate IO as well as
-- deferred IO which is executed in task list order.  Secondly, the library
-- permits various strategies to run tasks with a bounded number of
-- "processors".  Moreover, exceptions in parallel tasks are reported
-- in task order and are thus deterministic.  That is, an earlier tasks in the
-- task list will report its exception even if a later task completes with
-- a different exception first.
--
-- The task monad `PIO`'s immediate or "unobservable" IO `uIO` executes
-- immediately and is non-deterministic with respect to other tasks.  One might
-- use this to read a configuration file or spawn a process to run a test.
--
-- The second form of IO is deferred or "observable" by the parent task.
-- This set of IO actions are collected and replayed once the task finishes.
-- The typically application is to conditionally commit a result based on
-- prior task success.  For instance, suppose we are implementing a parallel
-- fail-fast testing environment.  We do not want a test to report a test
-- status if an earlier test fails first.  Thus, the failure can be wrapped
-- under `oIO` and it will only appear if prior tasks have not thrown an
-- `IO` style `Exception`.
--
-- A task is "observable" or "visible" after all tasks scheduled before
-- it are also "observable".
--
--
-- E.g. with a bound of 2 the four tasks might execute as follows.
--      (A and B are the processors, empty space indicates not started
--       | indicates completion, * indicates exception)
--  T1 AAAAA|
--  T2 BB|
--  T3   BBBBBBBBBB|
--  T4       AA|
--          ^ T1 observable (IO played and any exception propagated)
--          ^ T2 also visible, but we play it after T1
--                 ^ T3 visible
--
-- All observable IO is played on the calling thread.


-- The parallel IO monad.
-- Permits deferred IO computation through 'observableIO' (e.g. logging)
-- type PIO a = StateT PIOS IO a
newtype PIO a =
  PIO {
    pioStateT :: StateT PIOS IO a
  } deriving (Monad, Applicative, Functor, MonadIO)

-- We use an IORef so that we can safely catch IO exceptions within PIO and
-- retain the observable IO
--
-- Parallel IO state
data PIOS =
  PIOS {
    -- Deferred IO for this task
    piosIoRef :: IORef (IO ())
    --
    -- xIO (exclusive mutex)
  , piosMVar :: MVar ()
  }
instance MonadState PIOS PIO where
  get = PIO get
  put = PIO . put


-- FIXME: deferred IO gets flushed, we would need to
-- rethread the IORef into the child
-- unliftPIO :: PIO a -> IO a
-- unliftPIO = serialPIO


-- Run a parallel IO action returning both the value and the deferred
-- IO action.
runPIO :: PIOS -> PIO a -> IO (a, IO ())
runPIO pios (PIO pio) = do
  def_ioref <- newIORef (return ())
  (a,_) <- runStateT pio (pios{piosIoRef = def_ioref})
  evaluate a
  def_io <- readIORef def_ioref
  return (a,def_io)
runWithPiosPIO :: PIOS -> PIO a -> IO a
runWithPiosPIO pios (PIO pio) = do
  (a,_) <- runStateT pio pios
  evaluate a
  return a


newPIOS :: IO PIOS
newPIOS = newMVar () >>= newPIOS2
newPIOS2 :: MVar () -> IO PIOS
newPIOS2 mv_xio = do
  def_io_ref <- newIORef (return ())
  return $ PIOS def_io_ref mv_xio

-- Runs a PIO action and then the deferred IO
serialPIO :: PIOS -> PIO a -> IO a
serialPIO pio_st pio = do
  (a,oio) <- runPIO pio_st pio
  evaluate a -- flush out any exceptions first
  oio -- run the derferred IO
  return a

-- want to be able to fork within PIO (auto adjust queue size)
-- * parallel execute K tasks
-- * when a task is finished we can reply it's IO
-- * if a task throws an exception, we propagate that
--   exception once we reach that item.
--


-- Unobservable IO is executed immediately, but not ordered with respect
-- to the task list.
--
-- This is used for things like reading files, or writing unique files
-- that a task must do in parallel.
--
unobservableIO :: IO a -> PIO a
unobservableIO = liftIO
-- synonym for 'unobservableIO'
uIO :: IO a -> PIO a
uIO = unobservableIO
-- synonym for 'unobservableIO'
immediateIO :: IO a -> PIO a
immediateIO = unobservableIO


-- Observable and deterministic IO is deferred until the action completes.
-- One action's IO will be executed before any action after it in the PIO
-- list passed to runPIO.
--
-- This is used for things like logging error and debug messages during
-- the parallel task's execution and enables us to view it in a
-- determinstic manner.
observableIO :: IO a -> PIO ()
observableIO io = PIO $ do
  -- liftIO $ putStrLn "===========================>"
  -- liftIO $ io
  -- liftIO $ putStrLn "===========================<"
  def_io_ref <- gets piosIoRef
  liftIO $ modifyIORef def_io_ref $ \def_io -> def_io >> io >> return ()

-- synonym for 'observableIO'
oIO :: IO a -> PIO ()
oIO = observableIO
-- synonym for 'observableIO'
deferredIO :: IO a -> PIO ()
deferredIO = observableIO

-- exclusively execute within parallel IO action
-- Note, this serializes
exclusivePIO :: PIO a -> PIO a
exclusivePIO action = do
  mvar <- gets piosMVar
  let acq = immediateIO (takeMVar mvar)
      rel = immediateIO . putMVar mvar
  pioBracket acq rel (\_ -> action)
xPIO :: PIO a -> PIO a
xPIO = exclusivePIO
xIO :: IO a -> PIO a
xIO = exclusivePIO . immediateIO




asyncN :: Int -> [IO a] -> IO [a]
asyncN _  [] = return []
asyncN n0 ios0
  | n0 < 0 = error "asyncN: negative input"
  | n0 == 1 = sequence ios0
  | n0 == 0 = getNumCapabilities >>= runChunks [] ios0
  | otherwise = runChunks [] ios0 n0
  where runChunks :: [[a]] -> [IO a] -> Int -> IO [a]
        runChunks as []  _ = return (concat (reverse as))
        runChunks as ios n = do
          let (cs,rest) = splitAt n ios
          vs <- runChunk cs []
          runChunks (vs:as) rest n

        runChunk :: [IO a] -> [Async a] -> IO [a]
        runChunk [] as = mapM wait (reverse as)
        runChunk (io:ios) as = withAsync io $ \a -> runChunk ios (a:as)

asyncWithProgressN :: (Int -> IO ()) -> Int -> [IO a] -> IO [a]
asyncWithProgressN _  _  [] = return []
asyncWithProgressN cb n0 ios0
  | n0 < 0 = error "asyncWithProgressN: negative input"
  | n0 == 1 = sequence ios0
  | n0 == 0 = getNumCapabilities >>= runChunks 0 [] ios0
  | otherwise = runChunks 0 [] ios0 n0
  where runChunks :: Int -> [[a]] -> [IO a] -> Int -> IO [a]
        runChunks _ as []  _ = return (concat (reverse as))
        runChunks m0 as ios n = do
          let (cs,rest) = splitAt n ios
          vs <- runChunk cs []
          let m1 = m0 + length vs
          cb m1
          runChunks m1 (vs:as) rest n

        runChunk :: [IO a] -> [Async a] -> IO [a]
        runChunk [] as = mapM wait (reverse as)
        runChunk (io:ios) as = withAsync io $ \a -> runChunk ios (a:as)

asyncK :: Int -> [IO a] -> IO [a]
asyncK _  [] = return []
asyncK n ios0
  | n < 0 = error "asyncN: negative input"
  | n == 1 = sequence ios0
  | n == 0 = getNumCapabilities >>= flip asyncK ios0
  | otherwise = sequence (replicate n newEmptyMVar) >>= \mvrs -> runChunks mvrs ios0 []
  where runChunks :: [MVar (Either SomeException a)] -> [IO a] -> [[a]] -> IO [a]
        runChunks _   []   rss = (return . reverse . concat) rss
        runChunks mvrs ios rss = do
          let (cs,bs) = splitAt n ios
          mapM_ (forkIO . uncurry threadAction) (zip cs mvrs) -- starts chunk
          vs <- mapM takeMVar mvrs -- wait for chunk
          rs <- forM vs $ \v -> -- check for exceptions
                  case v of
                    Left e -> throw e
                    Right v -> return v
          runChunks mvrs bs (rs:rss)

        threadAction :: IO a -> MVar (Either SomeException a) -> IO ()
        threadAction ioa mv = do
          let handler :: SomeException -> IO (Either SomeException a)
              handler = return . Left

              runIO = ioa >>= evaluate
          val <- fmap Right runIO `catch` handler
          putMVar mv val


------------------------------------------------------------------------------
--
-- .....
--
------------------------------------------------------------------------------
--
-- based on http://hackage.haskell.org/package/MonadCatchIO-transformers-0.3.0.0/docs/src/Control-Monad-CatchIO.html#throw
-- instance MonadCatchIO m => MonadCatchIO (StateT s m) where
--  m `catch` f = StateT $ \s -> runStateT m s `catch` \e -> runStateT (f e) s

--
pioCatch :: Exception e => PIO a -> (e -> PIO a) -> PIO a
-- pioCatch :: PIO a -> (SomeException -> PIO a) -> PIO a
pioCatch (PIO state_a) pio_handler = do
  pios <- get
  PIO $ StateT $ \s -> do
    let -- handler :: SomeException -> IO (a,PIOS)
        handler e =
          case pio_handler e of
            PIO state_h -> runStateT state_h s
    runStateT state_a s `catch` handler

-- Throw an exception in the context of 'PIO'.
pioThrow :: Exception e => e -> PIO a
pioThrow = immediateIO . throwIO

-- Wraps an action in a handler and returns the result under `Right`
-- and any exception under `Left`.
pioTry :: Exception e => PIO a -> PIO (Either e a)
pioTry pio = (Right <$> pio) `pioCatch` (return . Left)

-- Similar to `finally` in Control.Exception.
-- This runs unconditionally.
pioFinally :: PIO a -> PIO b -> PIO a
pioFinally pio fin = pioBracket acq rel (const pio)
  where acq = (return ())
        rel = (\() -> fin)

-- Runs into the same problem since exception handler has to run in IO
--
-- pioBracket :: PIO a -> (a -> PIO b) -> (a -> PIO c) -> PIO c
-- pioBracket acq rel function = do
pioBracket :: PIO a -> (a -> PIO b) -> (a -> PIO c) -> PIO c
pioBracket acq rel action = do
  a <- acq
  let wrapped_action = do
        c <- action a
        rel a
        return c
      handler (e :: SomeException) = rel a >> pioThrow e
  wrapped_action `pioCatch` handler



--
-- pioWithFile :: FilePath -> IOMode -> (Handle -> PIO a) -> PIO a
-- pioWithFile fp mode function = pioBracket acq rel function
--  where acq = uIO (openFile fp mode) :: PIO FilePath
--        rel = uIO . hClose
pioWithFile :: FilePath -> IOMode -> (Handle -> PIO a) -> PIO a
pioWithFile fp mode function =
  pioBracket (uIO $ openFile fp mode) (uIO . hClose) function


putStrPIO :: String -> PIO ()
putStrPIO = observableIO . putStr
putStrLnPIO :: String -> PIO ()
putStrLnPIO = observableIO . putStrLn


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------



-- pseudo-sequentially executes PIO under whatever bound is required
runPIOs :: Int -> (b -> a -> IO b) -> b -> [PIO a] -> IO b
runPIOs _   _      b0 []   = return b0
runPIOs n0  reduce b0 pios
  | n0 <  0 = error "PIO.runPIOs: negative chunk size"
  | n0 == 0 = getNumCapabilities >>= startPIO
  | otherwise = startPIO n0
  where -- startPIO :: Int -> IO [a]
        startPIO n = do
          pio_st <- newPIOS
          runPIOsWithPIOS n0 pio_st reduce b0 pios

runListPIOs :: Int -> [PIO a] -> IO [a]
runListPIOs n = (reverse <$>) . runPIOs n (\as a -> return (a:as)) []

runPIOsWithPIOS :: Int -> PIOS -> (b -> a -> IO b) -> b -> [PIO a] -> IO b
runPIOsWithPIOS chunk_size pio_st reduce = runChunks
  where -- runChunks :: b -> [PIO a] -> IO [a]
        runChunks b  []   = return b
        runChunks b  pios = do
          let (pios_chunk,pios_sfx) = splitAt chunk_size pios
          -- putStrLn $ "running chunk of " ++ show (length pios_chunk)
          vs <- runChunk pios_chunk []
          sequence (map snd vs) -- run the deferred parts
          let as = map fst vs
          -- foldM :: (b -> a -> m b) -> b -> [a] -> m b
          --   reduceChunk :: b -> [a] -> IO b
          b1 <- foldM reduce b as
          -- unsafeInterleavedIO $ ...
          runChunks b1 pios_sfx

        -- runChunk :: [PIO a] -> [Async a] -> IO [(a,IO ())]
        runChunk []         ras = mapM wait (reverse ras)
        runChunk (pio:pios) ras =
          withAsync (runPIO pio_st pio) $ runChunk pios . (:ras)


--------------------------------------------------------------------------------
-- work stealing implementation of reduce
---------------------------
-- TODO: generalize to a version that takes a channels to suck in work

data WSParams =
  WSParams {
  -- The number of processors to use for processing work in parallel
  -- 0 means to use `getNumCapabilities` (number of cores on the machine).
  -- 1 is not a  serial implementation since the window parameter means
  --   the processor can still run ahead
    wspParallelism :: !Int
  --
  -- Approximate maximum number of outstanding tasks we permit before we
  -- throttle worker threads.  This can happen if the reduce step is
  -- sufficiently expensive that the main reducer thread cannot keep up with
  -- the processor threads.
  --
  -- A value of zero means there is no window constraint.
  , wspMaxWindow :: !Int
  --
  -- Upon failure do we wait for running tasks to stop or kill them immediately?
  , wspDrainOnFail :: !Bool
  --
  -- Whether or not to use bound threads (forkOS vs forkIO).
  , wspUseBoundThreads :: !Bool
  --
  -- If a task fails throws an exception, then do we play the partial IO?
  , wspPlayDeferredIoOnFail :: !Bool
  } deriving (Show,Eq)


wsp_dft :: WSParams
wsp_dft =
  WSParams {
    wspParallelism = 0
  , wspMaxWindow = 0
  , wspUseBoundThreads = True
  , wspDrainOnFail = False
  , wspPlayDeferredIoOnFail = True
  }


runWorkStealingPIOs :: WSParams -> (b -> a -> IO b) -> b -> [PIO a] -> IO b
runWorkStealingPIOs _   _       b0 [] = return b0
runWorkStealingPIOs wsp0 reducer b0 pios@(_:_) = do
  when (wspParallelism wsp0 < 0) $
    fail "runWorkStealingPIOs: invalid thread count"
  thrs <- if wspParallelism wsp0 == 0 then getNumCapabilities else return (wspParallelism wsp0)
  let wsp = wsp0 {wspParallelism = thrs}
  mv_dbg <- newMVar ()
  mv_ws <- newMVar $ WS (zip [(0 :: Int)..] pios) DIM.empty DIM.empty
  mv_xio <- newMVar ()
  runWorkStealingPIOsWithPIOS mv_dbg mv_ws mv_xio wsp reducer b0

data WSFailFastException = WSFailFastException
  deriving (Eq,Show,Typeable)
instance Exception WSFailFastException

data WS a =
  WS {
    -- when this is null processors can exit
    wsQ :: ![(Int,PIO a)]
    --
    -- the completed map (those pending reduction)
    -- processors: don't run ahead if the size is greater than max_window
  , wsRunning :: !WSRunningMap
    --
    -- those completed but awaiting reduction
  , wsCompleted :: !(WSCompletedMap a)
  }
type WSCompletedMap a = DIM.IntMap (Either SomeException a,IO ())
type WSRunningMap = DIM.IntMap ProcessorIndex
type ProcessorIndex = Int

-- The count of tasks that are in progress or completed and awaiting
-- reduction.
wsOutstanding :: WS a -> Int
wsOutstanding ws = DIM.size (wsRunning ws) + DIM.size (wsCompleted ws)

runWorkStealingPIOsWithPIOS ::
  MVar () ->
  MVar (WS a) ->
  MVar () ->
  WSParams ->
  (b -> a -> IO b) ->
  b ->
  IO b
runWorkStealingPIOsWithPIOS mv_dbg mv_ws mv_xio wsp reducer b0 =  do
    -- start worker threads
    let fork = if wspUseBoundThreads wsp then forkOS else forkIO
    mv_exits <- sequence $ replicate (wspParallelism wsp) newEmptyMVar
    let tixs = [(0 :: Int) .. (wspParallelism wsp - 1)]
    tids <- forM (zip tixs mv_exits) (fork . uncurry threadFunction)
    let waitForProcessors = do
          debugMaster "waiting for children to exit"
          mapM_ takeMVar mv_exits
    let interrupt :: SomeException -> IO a
        interrupt se = do
          debugMaster $ "caught unexpected " ++ show se
          modifyMVar_ mv_ws $ \ws -> return ws{wsQ = []}
          when (wspDrainOnFail wsp) $ do
            debugMaster "draining children"
          unless (wspDrainOnFail wsp) $ do
            debugMaster "killing children"
            mapM_ (flip throwTo WSFailFastException) tids
          throwIO se
    --
    b <- masterLoop mv_exits tids b0 (0 :: Int) (0 :: Int) `catch`
            interrupt `finally` waitForProcessors
    debugMaster "exiting normally"
    return b
  where masterLoop mv_exits tids b nxt_ix backoffs = do
          a <- masterPoll mv_exits nxt_ix
          -- debug $ "master polled " ++ show a
          case a of
            MasterThreadEXIT -> do
              debugMaster "exiting"
              return b
            MasterThreadRETRY -> do
              debugMaster $ "retry #" ++ show backoffs ++ " (waiting " ++ show (masterBackoffsToUs backoffs) ++ " us)"
              threadDelay (masterBackoffsToUs backoffs)
              masterLoop mv_exits tids b nxt_ix (backoffs + 1)
            MasterThreadREDUCE w_ix exa deferred_io -> do
              debugMaster $ "reducing #" ++ show nxt_ix
              let handler :: String -> SomeException -> IO c
                  handler when se = do
                    debugMaster $
                      "caught exception during " ++ when ++
                      " of #" ++ show w_ix ++ " " ++ show se
                    modifyMVar_ mv_ws $ \ws -> return ws{wsQ = []}
                    mapM_ (flip throwTo WSFailFastException) tids
                    throw se
              case exa of
                Left se -> do
                  debugMaster $ "detected exception; clearing queue"
                  modifyMVar_ mv_ws $ \ws -> return ws{wsQ = []}
                  when (wspPlayDeferredIoOnFail wsp) $ do
                    deferred_io `catch` handler "executing deferred IO (double fault)"
                  unless (wspDrainOnFail wsp) $ do
                    mapM_ (flip throwTo WSFailFastException) tids
                  throwIO se
                Right a -> do
                  deferred_io `catch` handler "executing deferred IO "
                  b1 <- reducer b a `catch` handler "reduction"
                  masterLoop mv_exits tids b1 (nxt_ix + 1) 0

        masterPoll mv_exits nxt_ix = do
          modifyMVar mv_ws $ \ws -> do
            xs <- mapM tryReadMVar mv_exits
            debugMaster $
              map (\x -> if x == Nothing then 'R' else 'X') xs ++ ": " ++ fmtWS ws
            return $ do
              case nxt_ix `DIM.lookup` wsCompleted ws of
                Nothing
                  | null (wsQ ws) &&
                    DIM.null (wsRunning ws) &&
                    DIM.null (wsCompleted ws) -> (ws,MasterThreadEXIT)
                  | otherwise -> (ws,MasterThreadRETRY)
                Just (a,def_io) -> (ws1,MasterThreadREDUCE nxt_ix a def_io)
                  where ws1 = ws{wsCompleted = DIM.delete nxt_ix (wsCompleted ws)}

        debugMaster :: String -> IO ()
        debugMaster msg = debugG $ "  master: " ++ msg

        debugG :: String -> IO ()
        debugG msg = return () -- doIt
          where doIt =
                  withMVar mv_dbg $ \_ -> do
                    putStrLn msg
                    hFlush stdout
                    return ()

        threadFunction :: Int -> MVar () -> IO ()
        threadFunction t_id mv_exit =
            threadBody `finally` putMVar mv_exit () -- no matter what (signal our exit MVar)
          where threadBody = do
                  let -- unplanned exception
                      handlerUnknown :: SomeException -> IO ()
                      handlerUnknown se = threadDebug ("exiting abnormally with unknown " ++ show se)
                      --
                      -- master thread is killing us (fail fast)
                  let handlerFailFast :: WSFailFastException -> IO ()
                      handlerFailFast _ = threadDebug "exiting with fail fast signal"

                      threadRun = do
                        pios <- newPIOS2 mv_xio
                        threadLoop pios 0 >> threadDebug "exiting naturally"
                  threadRun `catch` handlerFailFast `catch` handlerUnknown

                threadDebug :: String -> IO ()
                threadDebug msg = debugG ("  thread " ++ show t_id ++ ": " ++ msg)

                -- threadPoll :: IO (ThreadAction a)
                threadPoll =
                    modifyMVar mv_ws (return . func) -- do I need modifyMVarMasked?
                  where func ws
                          | null (wsQ ws) = (ws,ThreadActionEXIT)
                          | otherwise =
                            case wsQ ws of
                              (w_ix,pioa):wq_tail
                                | wspMaxWindow wsp > 0 && wsOutstanding ws == wspMaxWindow wsp ->
                                  (ws,ThreadActionRETRY)
                                | otherwise -> (ws1,ta)
                                where ws1 =
                                        ws {
                                          wsQ = wq_tail
                                        , wsRunning = DIM.insert w_ix t_id (wsRunning ws)
                                        }

                                      ta = ThreadActionRUN w_ix pioa

                threadLoop :: PIOS -> Int -> IO ()
                threadLoop pios backoffs = do
                  ta <- threadPoll
                  case ta of
                    ThreadActionEXIT -> return ()
                    ThreadActionRETRY -> do
                      threadDebug $
                        " retry #" ++ show backoffs ++
                        " (" ++ show (processorBackoffsToUs backoffs) ++ " us)"
                      threadDelay (processorBackoffsToUs backoffs)
                      threadLoop pios (backoffs + 1)
                    ThreadActionRUN w_ix pioa -> do
                        threadDebug ("starting W#" ++ show w_ix)
                        writeIORef (piosIoRef pios) (return ())
                        -- an exception in this try would probably indicates a
                        -- failure in the parallel task or the reduction (evaluate a)
                        -- transparently record the exception; if we are the first
                        -- in the completed actions, then it will be rethrown by our
                        -- parent thread
                        exa <- try (runWithPiosPIO pios pioa)
                        -- grab the defered IO regardless of failure
                        def_io <- readIORef (piosIoRef pios)
                        threadDebug $
                          "finished W#" ++ show w_ix ++ " " ++
                          (case exa of {Left se -> show se ++ " (exiting voluntarily)"; Right _ -> ""})
                        modifyMVar_ mv_ws $ \ws -> do
                          return
                            ws {
                              wsQ =
                                case exa of
                                  -- Early flag failure so other processors stop
                                  -- issuing work as fast as possible.
                                  -- The master thread may be asleep from
                                  -- a long backoff and this prevents other
                                  -- processors from starting  new work
                                  Left _ -> []
                                  Right _ -> wsQ ws
                            , wsRunning = DIM.delete w_ix (wsRunning ws)
                            , wsCompleted = DIM.insert w_ix (exa,def_io) (wsCompleted ws)
                            }
                        threadLoop pios 0 -- reset backoff

fmtWS :: WS a -> String
fmtWS ws =
    "WS {" ++
    "wsQ = " ++ ws_q ++ ", wsRunning = " ++ ws_r ++ ", wsCompleted = " ++ ws_c ++
    "}"
  where ws_q = fmtListElems (\(w_id,_) -> fmtWorkId w_id) $ wsQ ws
        ws_r = fmtListElems fmtR $ DIM.toList (wsRunning ws)
          where fmtR (w_id,t_id) = fmtWorkId w_id ++ "->T#" ++ show t_id
        ws_c = fmtListElems fmtC $ DIM.toList (wsCompleted ws)
          where fmtC (w_id,(exa,_)) = fmtWorkId w_id ++ "->" ++ val
                  where val = case exa of {Left _ -> "ER"; Right _ -> "ok"}
        fmtWorkId w_id = "W#" ++ show w_id
        fmtListElems fmtX xs = "[" ++ intercalate "," (map fmtX xs) ++ "]"


-- starting from 0
-- backs off 10 ms, 20 ms, 40 ms, stopping here
masterBackoffsToUs :: Int -> Int
masterBackoffsToUs x0 = 1000 * 10 * 2^x
  where x = min 3 (max 0 x0)

data MasterAction a =
    MasterThreadEXIT
  | MasterThreadRETRY
  | MasterThreadREDUCE !Int !(Either SomeException a) !(IO ())

instance Show (MasterAction a) where
  show MasterThreadEXIT = "MasterThreadEXIT"
  show MasterThreadRETRY = "MasterThreadRETRY"
  show (MasterThreadREDUCE ix exa io) =
      "MasterThreadREDUCE " ++ show ix ++ " " ++ txt ++ " ?"
    where txt =
            case exa of
              Right _ -> "(Right ?)"
              Left se -> "(Left " ++ show se ++ ")"

-- starting from 0
-- backs off 10 ms, 20 ms, 40 ms, 80 ms, 160 ms .... stopping here
processorBackoffsToUs :: Int -> Int
processorBackoffsToUs x0 = 1000 * 50 * 2^x -- millis 1, 2, 4, 8, 16, 32, 64
  where x = min 4 (max 0 x0)

data ThreadAction a =
    ThreadActionEXIT
  | ThreadActionRETRY
  | ThreadActionRUN !Int !(PIO a) -- next-completion -> wq-ix -> wq-elem



-----------------------------------------------
-- work stealing testing

tw :: String -> Int -> Int -> PIO Int
tw lbl val delay_ms = twG lbl val delay_ms False

twFail :: String -> Int -> PIO Int
twFail lbl delay_ms = twG lbl (error "should not see this") delay_ms True

twG :: String -> Int -> Int -> Bool -> PIO Int
twG lbl val delay_ms fail = do
  uIO $ threadDelay (1000 * delay_ms)
  when fail $
    uIO $ throwIO $ ErrorCall $ lbl ++ ": failing"
  return val


data TestScenario =
  TestScenario {
    tsLabel :: !String
  , tsDescription :: !String
  , tsBody :: (Bool -> IO String) -- debug arg
  }

data TestExpect = TestExpectSuccess Int
  deriving (Show,Eq)

tsRun :: TestScenario -> IO ()
tsRun ts = tsRunG True ts >> return ()

tsRunG :: Bool -> TestScenario -> IO Bool
tsRunG dbg ts = do
  let key = tsLabel ts ++ ":"
      padded_key = key ++ replicate (32 - length key) ' '
  if dbg then putStrLn $ padded_key ++ "  ..."
    else putStr $ padded_key ++ "  "
  res <- tsBody ts dbg
  when dbg $
    putStr $ padded_key ++ "  "
  if null res then putStrLn "SUCCESS"
    else putStrLn $ "FAILED: " ++ res
  return $ null res

go :: IO ()
go = tsRun ts_dfd_io_00

tsRunAll :: IO ()
tsRunAll = do
    zs <- mapM (\t -> tsRunG False t) tests
    let num_passed = length (filter id zs)
    putStrLn $ "passed " ++ show num_passed ++ "/" ++ show (length tests)
    when (num_passed /= length tests) $ error "some tests failed"
  where tests =
          [
            ts_dfd_io_00
          , ts_dfd_io_01
          , ts_fail_ord_00
          , ts_fail_ord_01
          , ts_drain_honored
          , ts_long_run_00
          , ts_long_run_01
          , ts_max_window_disabled
          , ts_max_window_enabled
          ]

-- deferred IO completes in the correct order
ts_dfd_io_00 :: TestScenario
ts_dfd_io_00 =
    mkTest "ts_dfd_io_00" "deferred IO completes in order #0"
      4 Nothing [0,1,2,3]
      [
        actP 4000 0
      , actP 3000 1
      , actP 2000 2
      , actP 1000 3
      ]
-- deferred IO completes in the correct order
ts_dfd_io_01 :: TestScenario
ts_dfd_io_01 =
    mkTest "ts_dfd_io_01" "deferred IO completes in order #1"
      4 (Just excp1) [0,1,2,3,4]
      [
        actP 5000 0 -- stalls for a long time
      , actP 10   1 -- runs fast
      , actP 10   2 -- runs fast
      , actP 10   3 -- runs fast
      , actP 10   4 -- runs fast
      , actF 1000  excp1 -- fails first
      , actP 10   5 -- runs fast
      , actP 10   6 -- runs fast
      ]
    where excp1 = SomeException (ErrorCall "exception1")
-- exceptions complete in the correct order
ts_fail_ord_00 :: TestScenario
ts_fail_ord_00 =
    mkTest "ts_fail_ord_00" "first logical failure is returned"
      4 (Just excp1) [0,1]
      [
        actP 10   0 -- runs fast
      , actP 10   1 -- runs fast
      , actF 2000  excp1 -- fails second, but is propagated
      , actP 10   3 -- runs fast
      , actF 10    excp2 -- finishes first, but doesn't appear
      ]
    where excp1 = SomeException (ErrorCall "exception1")
          excp2 = SomeException (ErrorCall "exception2")
-- exceptions complete in the correct order
ts_fail_ord_01 :: TestScenario
ts_fail_ord_01 =
    mkTest "ts_fail_ord_01" "first logical failure is returned (exception in deferred IO)"
      4 (Just excp1) [0,1]
      [
        actP 10   0 -- runs fast
      , actP 10   1 -- runs fast
      , actDefdF 2000 2 excp1 -- fails second, but is propagated
      , actP 10   3 -- runs fast
      , actF 10    excp2 -- finishes first, but doesn't appear
      ]
    where excp1 = SomeException (ErrorCall "exception1")
          excp2 = SomeException (ErrorCall "exception2")

ts_drain_honored :: TestScenario
ts_drain_honored =
    mkTestG "ts_drain_honored" "ensures that drain-on-fail is honored"
      wsp_dft{wspParallelism = 2,wspDrainOnFail = True}
      (newMVar (0 :: Int)) checkMv
      (Just excp1) []
      [
        \_ -> actF 1000 excp1
      , actIncr 2000 32 -- should drain [runs parallel to #0]
      -- we can't prove actions after this won't run because master thread
      -- might run slowly while t#1 and t#2 continue
      --
      -- is a potential race in this test except 2 threads saves us
      , actIncr   10 99 -- should NOT run drain
      ]
    where excp1 = SomeException (ErrorCall "exception1")

          actIncr :: Int -> Int -> (MVar Int -> PIO Int)
          actIncr delay_ms id = \mv -> do
            x <- actP delay_ms id
            uIO (modifyMVar_ mv (return . (+id)))
            return x

          checkMv :: MVar Int -> IO ()
          checkMv mv = do
            n <- readMVar mv
            when (n /= (32+10) && n /= (32)) $ fail ("action didn't drain: " ++ show n)

-- want to study perf
ts_long_run_00 :: TestScenario
ts_long_run_00 =
      mkTest "ts_long_run_00" "tests a long running sequence of actions"
        4 Nothing range
        (map (actP 10) range)
    where range = [0..999]
ts_long_run_01 :: TestScenario
ts_long_run_01 =
      mkTest "ts_long_run_01" "tests a long running sequence of actions with varying lengths"
        4 Nothing range
        (zipWith actP (cycle cyc_ms) range)
    where range = [0..99]
          cyc_ms = [500,20,10,50] ++ reverse [500,20,100,50]

ts_max_window_disabled :: TestScenario
ts_max_window_disabled =
  mkWindowTest False
    "ts_max_window_disabled" "tests no maximum window size"

ts_max_window_enabled :: TestScenario
ts_max_window_enabled =
    mkWindowTest True
      "ts_max_window_enabled" "tests a maximum window size"

-- This test ensures whe max-window value throttles processor threads correctly.
--
-- Creates `N + 1` actions and sets the window size to:
-- either `0` (infinite) or `N`.  The first and last actions store their
-- ID (0 or N).  Depending on this choice the first/last action,
-- respectively, will write their value to the MVar last.
mkWindowTest :: Bool -> String -> String -> TestScenario
mkWindowTest use_window lbl desc =
      mkTestG
        lbl desc
        (wsp_dft {wspParallelism = 2,wspMaxWindow = window_size})
        (newMVar (-1 :: Int)) checkMv
        Nothing range actions
    where range = [0 .. last_id]

          window_size :: Int
          window_size = if use_window then last_id else 0

          last_id :: Int
          last_id = 8 -- 20

          actions :: [MVar Int -> PIO Int]
          actions =
              act0 : map (\id -> \_ -> actP 0 id) range_fast_part ++ [act_last]
            where range_fast_part :: [Int]
                  range_fast_part = [1 .. last_id - 1]

                  act0 :: MVar Int -> PIO Int
                  act0 mv = do
                    x <- actP 3000 0
                    uIO $ modifyMVar_ mv (return . const 0)
                    return x

                  act_last :: MVar Int -> PIO Int
                  act_last mv = do
                    uIO $ modifyMVar_ mv (return . const last_id)
                    return last_id

          checkMv :: MVar Int -> IO ()
          checkMv mv = do
            n <- readMVar mv
            let expect = if use_window then last_id else 0
            mval <- tryReadMVar mv
            when (mval /= Just expect) $
              fail ("wrong value in Mvar (window not honored?); " ++ show mval)

actP :: Int -> a -> PIO a
actP delay_ms ret = act delay_ms (Right ret)
actF :: Int -> SomeException -> PIO a
actF delay_ms x = act delay_ms (Left x)

actDefdF :: Int -> a -> SomeException -> PIO a
actDefdF delay_ms a x = do -- exception in observable IO
  oIO (throwIO x) -- throw, but not rigth away
  uIO $ threadDelay (1000 * delay_ms)
  return a

act :: Int -> Either SomeException a -> PIO a
act delay_ms exa = do
  uIO $ threadDelay (1000 * delay_ms)
  case exa of
    Left x -> uIO (throwIO x)
    Right r -> return r

mkTest :: (Show a, Eq a) =>
  String -> String ->
  Int ->
  Maybe SomeException -> [a] -> [PIO a] -> TestScenario
mkTest lbl desc n m_except expected_vals seq =
    mkTestG lbl desc
      wsp_dft{wspParallelism = n} (return ()) (\() -> return ())
      m_except expected_vals seq_wrapped
  where seq_wrapped = map wrapAction seq
          where wrapAction act = \() -> act

-- a sequential test that runs a list of parallel actions
-- takes:
--    * expected partial output [a] to match; even on fail this will be matched
--    * possible exception of the top-level call (Maybe SomeException)
mkTestG :: (Show a, Eq a) =>
  String -> String ->
  WSParams ->
  IO s -> (s -> IO ()) ->
  Maybe SomeException -> [a] ->
  [s -> PIO a] ->
  TestScenario
mkTestG lbl desc wsp setup shutdown m_except expected_vals act_seq =
    TestScenario lbl desc func
  where func dbg = do
          s <- setup
          -- we use an MVar rather than regular plumbing because we want to
          -- capture partial scans
          mv_ord <- newMVar []
          --
          let -- reduceOrd :: [a] -> a -> IO [a]
              reduceOrd as a = do
                  modifyMVar_ mv_ord (return . (a:))
                  return (a:as)
          --
          -- index the parallel actions so we can report their completion order for debugging
          mv_unord <- newMVar []
          let ixd_acts = zipWith f [0..] act_seq
                where f ix act = wrapped_act
                        where wrapped_act = do
                                exa <- pioTry (act s)
                                uIO $ modifyMVar_ mv_unord (return . ((ix,exa):))
                                case exa of
                                  Left err -> pioThrow (err :: SomeException)
                                  Right a -> return a
          exa <- try (reverse <$> runWorkStealingPIOs wsp reduceOrd [] ixd_acts)
          vals <- reverse <$> readMVar mv_ord
          --
          length (show exa) `seq` return ()
          length (show vals) `seq` return ()
          --
          when dbg $ do
            uvals <- reverse <$> readMVar mv_unord
            let fmtC (w_ix,exa) = "  #" ++ show w_ix ++ ". " ++ str ++ "\n"
                  where str =
                          case exa of
                            Left se -> show se
                            Right a -> show a
            putStrLn $
              "  run returned:      " ++ show exa ++ "\n" ++
              "  logical order:     " ++ show vals ++ "\n" ++
              "  completion order:  " ++ show uvals ++
              ""

          let handler se = return $ show (se :: SomeException)
          err <- (shutdown s >> return "") `catch` handler
          if not (null err) then return ("shutdown threw " ++ err)
            else do
              let comparePartialValues =
                      if vals /= expected_vals
                        then return ("return values doesn't match expected: " ++ show vals)
                        else return "" -- matched
              case (m_except,exa) of
                (Nothing,Right retvals)
                  | retvals == expected_vals -> comparePartialValues -- also compare the reduce order
                  | otherwise -> return ("returned values are wrong: " ++ show retvals)
                (Just se1,Left se2)
                  | show se1 == show (se2 :: SomeException) -> comparePartialValues -- compare the part reduced
                  | otherwise -> return ("wrong exception " ++ show se2)
                (Just _,Right _) -> return ("failed to raise an exception: " ++ show vals)
                (Nothing,Left se) -> return ("unexpected exception " ++ show se)

-- runWorkStealingPIOs :: WSParams -> (b -> a -> IO b) -> b -> [PIO a] -> IO b


fib :: Integer -> Integer
fib n = if n <= 1 then 1 else fib (n - 1) + fib (n - 2)
fact n = if n == 0 then 1 else (n * fact (n-1))
{-
FIXME: asyncN only uses 12.5%

-- fibs :: [IO Integer]
-- fibs = map prFib
--  where doFib n = return $! fib n

inps = [32, 28, 31, 29]

ios = map prFib (inps++inps++inps)

fib :: Integer -> Integer
fib n = if n <= 1 then 1 else fib (n - 1) + fib (n - 2)

prFib n = do
  let hdr = (show n ++ " -> ...")
  show (length hdr) `seq` return ()
  putStrLn hdr
  let str = show n ++ " -> " ++ show (fib n)
  show (length str) `seq` return ()
  putStrLn str

act = forkOS $ print (fib 34)
acts = sequence $ replicate 8 act
-}






{-
TODO: can we implement a barrier???
   nothing can start until everything before the barrier is completed
   what does this even mean (action in a PIO monad?)

-- tests
--    * test that deferred IO is issued in task order
--
--    * test that deferred IO before a failure completes
--
--    * test that deferred IO after a failed task does not run
--
--    * ensure the correct exception is throw (first logically)
--
--    * drain on failure (T0 5s and then crash, T1)
--
--    * fails wait until priors complete
--        T0 runs near-forever, T1 fails after a few seconds
--        ==> reduction sees T0 then finally T1 fail
--    * fails cancel successors
--        T0 runs 5s and crashes, T1 runs 10s
--        ==> T1 reduction never encountered
--    * work stealing works
--        T0 runs 30s, T1...31 all take almost no time
--    * infinite list (should not leak)
--    * max_window size is honored: set window to 4
--         T0 runs 30s, T1..T31 runs immediately
--          => T4 doesn't reduce until after T
--    * failure cancels pending tasks immediately
--

-- OPTIONS:
--   - soft error drain (upon error let workers exit naturally)
-}
