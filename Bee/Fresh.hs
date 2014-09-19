{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Fresh (
 Fresh(..), runFresh, fresh
 ) where

import Control.Monad.State

newtype Fresh s a = Fresh { unFresh :: State [s] a }
 deriving Monad

runFresh m s = evalState (unFresh m) s

fresh = do (x:xs) <- Fresh get ; Fresh (put xs) ; return x
