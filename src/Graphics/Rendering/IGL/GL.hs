-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
----------------------------------------------------------------------------

module Graphics.Rendering.IGL.GL (
    -- * GL monad
    GL
  , runGL
    -- * Wrap arbitrary IO actions into GL
  , wrapGL
    -- * Context shit
  , Key
  , Nil
  , Has(..)
  , Lookup(..)
  , Insert(..)
  , Delete(..)
  , Bound
  ) where

import Control.Monad.Indexed

newtype GL i j a = GL { runGL :: IO a }

instance IxFunctor GL where
  imap f (GL gl) = GL $ fmap f gl

instance IxPointed GL where
  ireturn = GL . pure

instance IxApplicative GL where
  GL f `iap` GL x = GL $ f <*> x

instance IxMonad GL where
  f `ibind` GL x= GL $ x >>= runGL . f

wrapGL :: IO a -> GL i j a
wrapGL = GL
{-# INLINE wrapGL #-}

data Key k v kvs
data Nil

type family Has k ctx :: Bool where
  Has k (Key k v ctx) = True
  Has k (Key k' v ctx) = Has k ctx
  Has k Nil = False

type family Lookup k ctx :: Maybe v where
  Lookup k (Key k v ctx) = Just v
  Lookup k (Key k' v ctx) = Lookup k ctx
  Lookup k Nil = Nothing

type family Insert k v ctx :: * where
  Insert k v (Key k' v' ctx) = Key k v (Key k' v' ctx)
  Insert k v Nil = Key k v Nil

type family Delete k ctx :: * where
  Delete k (Key k v ctx) = ctx
  Delete k (Key k' v ctx) = Key k' v (Delete k ctx)

data Bound a
