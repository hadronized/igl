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

module Graphics.Rendering.IGL.Framebuffer (
    -- *
  ) where

import Foreign.Marshal.Array
import Graphics.GL
import Graphics.Rendering.IGL.GL
import Numeric.Natural ( Natural )

newtype Framebuffer = Framebuffer { framebufferID :: GLuint }

genFramebuffers :: Natural -> GL i i [Framebuffer]
genFramebuffers n = wrapGL $ do
  allocaArray (fromIntegral n) $ \p -> do
    glGenFramebuffers (fromIntegral n) p
    framebuffers <- peekArray (fromIntegral n) p
    pure $ map Framebuffer framebuffers

deleteFramebuffers :: [Framebuffer] -> GL i i ()
deleteFramebuffers framebuffers = wrapGL $ do
  withArrayLen (map framebufferID framebuffers) $
    glDeleteFramebuffers . fromIntegral

class FramebufferTarget t where
  fromFramebufferTarget :: t -> GLenum

data ReadFramebuffer = ReadFramebuffer deriving (Eq,Show)
data WriteFramebuffer = WriteFramebuffer deriving (Eq,Show)
data ReadWriteFramebuffer = ReadWriteFramebuffer deriving (Eq,Show)

instance FramebufferTarget ReadFramebuffer where
  fromFramebufferTarget = const GL_READ_FRAMEBUFFER
instance FramebufferTarget WriteFramebuffer where
  fromFramebufferTarget = const GL_DRAW_FRAMEBUFFER
instance FramebufferTarget ReadWriteFramebuffer where
  fromFramebufferTarget = const GL_FRAMEBUFFER

class FramebufferAttachment a where
  fromFramebufferAttachment :: a -> GLenum

data ColorAttachment = ColorAttachment Natural deriving (Eq,Show)
data DepthAttachment = DepthAttachment deriving (Eq,Show)
data StencilAttachment = StencilAttachment deriving (Eq,Show)
data DepthStencilAttachment = DepthStencilAttachment deriving (Eq,Show)

instance FramebufferAttachment ColorAttachment where
  fromFramebufferAttachment (ColorAttachment i) = GL_COLOR_ATTACHMENT0 + fromIntegral i
instance FramebufferAttachment DepthAttachment where
  fromFramebufferAttachment = const GL_DEPTH_ATTACHMENT
instance FramebufferAttachment StencilAttachment where
  fromFramebufferAttachment = const GL_STENCIL_ATTACHMENT
instance FramebufferAttachment DepthStencilAttachment where
  fromFramebufferAttachment = const GL_DEPTH_STENCIL_ATTACHMENT

--framebufferTexture1D :: FramebufferTarget -> FramebufferAttachment -> TextureTarget -> Texture -> Natural -> GL ? ? ()
