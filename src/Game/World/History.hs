{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Game.World.History where

import qualified Data.IntMap.Strict  as M
import qualified Data.Vector.Unboxed as U
import           Data.IORef
  ( IORef
  , newIORef
  , readIORef
  , writeIORef
  )
import           Control.Monad.IO.Class (liftIO)
import           Data.Maybe             (fromJust)
import           Control.Monad          (when)
import           Apecs.Core
import           Apecs.Stores

-- Notes for making custom stores

-- The `Elem` type-level function gives the type element of an
-- element of a store

-- For a component definition to be valid, `Elem` & `Storage` must
-- be inverses of one another, by definition because of the
-- `Elem (Storage c) ~ c` in:
--
-- class (Elem (Storage c) ~ c) => Component c where
--  type Storage c


data History s = History (IORef (M.IntMap (IORef (Elem s)))) s

type instance Elem (History s) = Elem s

instance ExplInit IO s => ExplInit IO (History s) where
  explInit = do
    -- create empty intmap to represent history store
    history <- newIORef mempty
    -- initialize wrapped storage
    wrapped <- explInit
    return $ History history wrapped

instance ExplGet IO s => ExplGet IO (History s) where
  -- defer explGet and explExists calls to the wrapped map
  {-# INLINE explExists #-}
  explExists (History _ s) ety = explExists s ety
  {-# INLINE explGet    #-}
  explGet    (History _ s) ety = explGet    s ety

instance (ExplGet IO s, ExplSet IO s) => ExplSet IO (History s) where
  {-# INLINE explSet #-}
  explSet (History ref s) ety x = do
    -- check that it exists in the wrapped map
    e <- explExists s ety
    when e $ do
      -- get current value
      val <- explGet s ety
      -- get the wrapper's intmap
      m   <- readIORef ref
      case M.lookup ety m of
        -- if it does not currently exist in the history
        Nothing -> do
          -- create new IORef to insert into the map
          rInsert   <- newIORef val
          writeIORef ref $ M.insert ety rInsert m
        -- if lookup yields a value, step the values
        Just cref -> writeIORef cref val
    -- also do it to the wrapped store
    explSet s ety x

-- destroy & members are straightforward
instance (ExplGet IO s, ExplDestroy IO s) => ExplDestroy IO (History s) where
  {-# INLINE explDestroy #-}
  explDestroy (History ref s) ety = do
    e <- explExists s ety
    when e $ do
    -- delete it in the history
      readIORef ref >>= writeIORef ref . M.delete ety
    -- delete it in wrapped map
    explDestroy s ety

instance ExplMembers IO s => ExplMembers IO (History s) where
  {-# INLINE explMembers #-}
  explMembers (History _ s) = explMembers s

getHistory :: forall w s c.
  ( Component c
  , Has w IO  c
  , Storage c ~ History s
  , c ~ Elem s
  )
  => Entity -> System w (Maybe c)
getHistory (Entity ety) = do
  History ref _ :: History s <- getStore
  -- get intmap from history store ref
  m                          <- liftIO $ readIORef ref
  case M.lookup ety m of
    Nothing   -> return Nothing
    Just cref -> do
      val <- liftIO $ readIORef cref
      return $ Just val