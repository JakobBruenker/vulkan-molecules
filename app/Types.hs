module Types where

import RIO (MVar)

type HasKillCompute = ?killCompute :: MVar ()
type HasPauseCompute = ?pauseCompute :: MVar ()
