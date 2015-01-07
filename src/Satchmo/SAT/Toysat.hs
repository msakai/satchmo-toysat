{-# LANGUAGE ScopedTypeVariables, TypeFamilies, MultiParamTypeClasses, TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall #-}
--------------------------------------------------------------------
-- |
-- Copyright :  © Masahiro Sakai 2014
-- License   :  BSD3
-- Maintainer:  Masahiro Sakai <masahiro.sakai@gmail.com>
-- Stability :  experimental
-- Portability: portable
--
--------------------------------------------------------------------

module Satchmo.SAT.Toysat

( SAT
, fresh
, emit
, solve
, solve_with_timeout
)

where

import qualified ToySolver.SAT as API

import Satchmo.Data
import Satchmo.Boolean hiding ( not )
import Satchmo.Code

import Control.Concurrent
import Control.Exception
import Control.Monad.Fix
import Control.Applicative
import Data.Array.IArray
import System.IO

newtype SAT a 
      = SAT { unSAT :: API.Solver -> IO a
            } 

instance Functor SAT where
    fmap f ( SAT m ) = SAT $ \ s -> fmap f ( m s )

instance Monad SAT where
    return x = SAT $ \ _ -> return x
    SAT m >>= f = SAT $ \ s -> do 
        x <- m s ; let { SAT n = f x } ; n s

instance Applicative SAT where
    pure = return
    a <*> b = a >>= \ f -> fmap f b

instance MonadFix SAT where
    mfix f = SAT $ \ s -> mfix ( \ a -> unSAT (f a) s )

instance MonadSAT SAT where
  fresh = SAT $ \ s -> do 
      x <- API.newVar s
      let l = literal True x
      -- hPutStrLn stderr $ "fresh: " ++ show (x, l)
      return l

  emit cl = SAT $ \ s -> do
      let conv l = if positive l then variable l else negate (variable l) 
          apicl = map conv $ literals cl
      API.addClause s apicl
      -- hPutStrLn stderr $ "adding clause " ++ show (cl, apicl, res)
      return ()

  note msg = SAT $ \ _ -> hPutStrLn stderr msg

  type Decoder SAT = SAT 
  decode_variable v = SAT $ \ s -> do
      m <- API.getModel s
      return $ m ! v
      
instance Decode SAT Boolean Bool where
    decode b = case b of
        Constant c -> return c
        Boolean  l -> do 
            let dv v = SAT $ \ s -> do
                    m <- API.getModel s
                    return $ m ! v
            v <- dv $ variable l
            return $ if positive l then v else not v

solve_with_timeout :: Maybe Int -> SAT (SAT a) -> IO (Maybe a)
solve_with_timeout mto action = do
    accu <- newEmptyMVar 
    worker <- forkIO $ do res <- solve action ; putMVar accu res
    timer <- forkIO $ case mto of
        Just to -> do 
              threadDelay ( 10^(6::Int) * to ) 
              killThread worker 
              putMVar accu Nothing
        _  -> return ()
    takeMVar accu `Control.Exception.catch` \ ( _ :: AsyncException ) -> do
        hPutStrLn stderr "caught"
        killThread worker
        killThread timer
        return Nothing

solve :: SAT (SAT a) -> IO (Maybe a)
solve action = do
    s <- API.newSolver
    hPutStrLn stderr $ "start producing CNF"
    SAT decoder <- unSAT action s
    v <- API.nVars s
    c <- API.nConstraints s
    hPutStrLn stderr 
        $ unwords [ "CNF finished", "vars", show v, "clauses", show c ]
    hPutStrLn stderr $ "starting solver"
    status <- API.solve s
    hPutStrLn stderr $ "solver finished, result: " ++ show status
    if status  then do
        hPutStrLn stderr $ "starting decoder"    
        out <- decoder s
        hPutStrLn stderr $ "decoder finished"    
        return $ Just out
    else return Nothing
