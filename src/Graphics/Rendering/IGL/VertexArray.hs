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

module Graphics.Rendering.IGL.VertexArray (
    -- *
  ) where

import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Graphics.GL
import Graphics.Rendering.IGL.GL
import Numeric.Natural ( Natural )

newtype VertexArray = VertexArray { vertexArrayID :: GLuint }

genVertexArrays :: Natural -> GL i i [VertexArray]
genVertexArrays n = wrapGL $ do
  allocaArray (fromIntegral n) $ \p -> do
    glGenVertexArrays (fromIntegral n) p
    vas <- peekArray (fromIntegral n) p
    pure $ map VertexArray vas

deleteVertexArrays :: [VertexArray] -> GL i i ()
deleteVertexArrays vas = wrapGL $
  withArrayLen (map vertexArrayID vas) $ glDeleteVertexArrays . fromIntegral

bindVertexArray :: (Has (Bound VertexArray) i ~ 'False)
                => VertexArray
                -> GL i (Insert (Bound VertexArray) 'True i) ()
bindVertexArray = wrapGL . glBindVertexArray . vertexArrayID

unbindVertexArray :: (Has (Bound VertexArray) i ~ 'True)
                  => GL i (Delete (Bound VertexArray) i) ()
unbindVertexArray = wrapGL $ glBindVertexArray 0

-- TODO: can’t we track the array?
enableVertexAttribArray :: (Has (Bound VertexArray) i ~ 'True)
                        => Natural
                        -> GL i i ()
enableVertexAttribArray = wrapGL . glEnableVertexAttribArray . fromIntegral

disableVertexAttribArray :: (Has (Bound VertexArray) i ~ 'True)
                         => Natural
                         -> GL i i ()
disableVertexAttribArray = wrapGL . glDisableVertexAttribArray . fromIntegral
