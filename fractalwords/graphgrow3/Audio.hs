module Audio (audioNew, Audio) where

import System.IO (Handle, hPutStr, hFlush)
import Data.IORef (IORef, newIORef, atomicModifyIORef)

import Control.Concurrent (threadDelay)

import Data.Map (Map)
import qualified Data.Map.Strict as M
import qualified Data.Map.Strict.Merge as M

import System.Process (createProcess, proc, close_fds, std_in, terminateProcess, StdStream(CreatePipe))

import Paths_graphgrow3 (getDataFileName)
import Types (NID, RID)

type Audio = (Int, [(NID, NID, RID, Double, Double)])

data AudioEngine = AudioEngine
  { aOld :: IORef (Map (RID, NID, NID) (RID, Double, Double))
  , aPull :: IO (Map RID Audio)
  , aPdSend :: Handle
  }

audioNew :: IO (Map RID Audio) -> IO (IO (), IO ())
audioNew pull = do
  aOld' <- newIORef M.empty
  patch <- getDataFileName "graphgrow3.pd"
  (_, _, _, pd) <- createProcess (proc "pd" ["-nogui", patch]){ close_fds = True }
  threadDelay (1000 * 1000)
  (Just h, _, _, pdsend) <- createProcess (proc "pdsend"  ["6060"]){ close_fds = True, std_in = CreatePipe }
  return (audio AudioEngine
    { aOld = aOld'
    , aPull = pull
    , aPdSend = h
    }, terminateProcess pdsend >> terminateProcess pd)

audio :: AudioEngine -> IO ()
audio a = do
  m <- aPull a
  let m' = M.fromList
              [ ((r0, min n0 n1, max n0 n1), (r1, s, x))
              | (r0, (_, es)) <- M.toList m
              , (n0, n1, r1, s, x) <- es
              ]
  m0' <- atomicModifyIORef (aOld a) $ \m0' -> (m', m0')
  let toDelete = m0' `M.difference` m'
      toAdd    = m' `M.difference` m0'
      toUpdate = M.merge
                  (M.mapMissing $ \_ (_, s1, x1) -> (Nothing, Just s1, Just x1))
                  M.dropMissing
                  (M.zipWithMatched $ \_ (r1, s1, x1) (r0, s0, x0) ->
                    ( if r1 == r0 then Nothing else Just r1
                    , if s1 == s0 then Nothing else Just s1
                    , if x1 == x0 then Nothing else Just x1
                    )
                  )
                  m' m0'
      deletes = unlines
        [ "delete " ++ (unwords $ map show [r0, n0, n1]) ++ ";"
        | (r0, n0, n1) <- M.keys toDelete
        ]
      adds = unlines
        [ "add " ++ (unwords $ map show [r0, n0, n1, r1]) ++ ";"
        | ((r0, n0, n1), (r1, _, _)) <- M.toList toAdd
        ]
      updateTargets = unlines
        [ "set " ++ (unwords $ map show [r0, n0, n1]) ++ " target " ++ show r1 ++ ";"
        | ((r0, n0, n1), (Just r1, _, _)) <- M.toList toUpdate
        ]
      updateScales = unlines
        [ "set " ++ (unwords $ map show [r0, n0, n1]) ++ " scale " ++ show s ++ ";"
        | ((r0, n0, n1), (_, Just s, _)) <- M.toList toUpdate
        ]
      updatePans = unlines
        [ "set " ++ (unwords $ map show [r0, n0, n1]) ++ " pan " ++ show x ++ ";"
        | ((r0, n0, n1), (_, _, Just x)) <- M.toList toUpdate
        ]
      packet = deletes ++ adds ++ updateTargets ++ updatePans ++ updateScales
  hPutStr (aPdSend a) packet
  hFlush (aPdSend a)
