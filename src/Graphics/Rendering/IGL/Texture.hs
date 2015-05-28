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

module Graphics.Rendering.IGL.Texture (
    -- *
  ) where

import Foreign.Marshal.Array
import Graphics.GL
import Graphics.Rendering.IGL.GL
import Numeric.Natural ( Natural )

newtype Texture = Texture { textureID :: GLuint }

genTextures :: Natural -> GL i i [Texture]
genTextures n = wrapGL $ do
  allocaArray (fromIntegral n) $ \p -> do
    glGenTextures (fromIntegral n) p
    textures <- peekArray (fromIntegral n) p
    pure $ map Texture textures

deleteTextures :: [Texture] -> GL i i ()
deleteTextures textures = wrapGL $ do
  withArrayLen (map textureID textures) $ glDeleteTextures . fromIntegral

class TextureTarget t where
  fromTextureTarget :: t -> GLenum

data Texture1D = Texture1D deriving (Eq,Show)
data Texture2D = Texture2D deriving (Eq,Show)
data Texture3D = Texture3D deriving (Eq,Show)
data TextureRectangle = TextureRectangle deriving (Eq,Show)
data TextureBuffer = TextureBuffer deriving (Eq,Show)
data TextureCubeMap = TextureCubeMap deriving (Eq,Show)
data Texture1DArray = Texture1DArray deriving (Eq,Show)
data Texture2DArray = Texture2DArray deriving (Eq,Show)
data TextureCubeMapArray = TextureCubeMapArray deriving (Eq,Show)
data Texture2DMultiSample = Texture2DMultiSample deriving (Eq,Show)
data Texture2DMultiSampleArray = Texture2DMultiSampleArray deriving (Eq,Show)


instance TextureTarget Texture1D where
 fromTextureTarget = const GL_TEXTURE_1D
instance TextureTarget Texture2D where
 fromTextureTarget = const GL_TEXTURE_2D
instance TextureTarget Texture3D where
 fromTextureTarget = const GL_TEXTURE_3D
instance TextureTarget TextureRectangle where
 fromTextureTarget = const GL_TEXTURE_RECTANGLE
instance TextureTarget TextureBuffer where
 fromTextureTarget = const GL_TEXTURE_BUFFER
instance TextureTarget TextureCubeMap where
 fromTextureTarget = const GL_TEXTURE_CUBE_MAP
instance TextureTarget Texture1DArray where
 fromTextureTarget = const GL_TEXTURE_1D_ARRAY
instance TextureTarget Texture2DArray where
 fromTextureTarget = const GL_TEXTURE_2D_ARRAY
instance TextureTarget TextureCubeMapArray where
 fromTextureTarget = const GL_TEXTURE_CUBE_MAP_ARRAY
instance TextureTarget Texture2DMultiSample where
 fromTextureTarget = const GL_TEXTURE_2D_MULTISAMPLE
instance TextureTarget Texture2DMultiSampleArray where
 fromTextureTarget = const GL_TEXTURE_2D_MULTISAMPLE_ARRAY
