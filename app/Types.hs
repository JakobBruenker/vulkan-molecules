module Types where

import RIO (MVar)

type HasKillCompute = ?killCompute :: MVar ()
type HasContinueCompute = ?continueCompute :: MVar ()
