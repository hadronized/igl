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

module Graphics.Rendering.IGL.Buffer (
    -- *
  ) where

import Control.Monad.Indexed
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Graphics.GL
import Graphics.Rendering.IGL.GL
import Numeric.Natural ( Natural )

newtype Buffer = Buffer { bufferID :: GLuint }

class BufferTarget t where
  fromBufferTarget :: t -> GLenum

data ArrayBuffer = ArrayBuffer deriving (Eq,Show)
data ElementArrayBuffer = ElementArrayBuffer deriving (Eq,Show)
data CopyReadBuffer = CopyReadBuffer deriving (Eq,Show)
data CopyWriteBuffer = CopyWriteBuffer deriving (Eq,Show)
data PixelUnpackBuffer = PixelUnpackBuffer deriving (Eq,Show)
data PixelPackBuffer = PixelPackBuffer deriving (Eq,Show)
data QueryBuffer = QueryBuffer deriving (Eq,Show)
data TextureBuffer = TextureBuffer deriving (Eq,Show)
data TransformFeedbackBuffer = TransformFeedbackBuffer deriving (Eq,Show)
data UniformBuffer = UniformBuffer deriving (Eq,Show)
data DrawIndirectBuffer = DrawIndirectBuffer deriving (Eq,Show)
data AtomicCounterBuffer = AtomicCounterBuffer deriving (Eq,Show)
data DispatchIndirectBuffer = DispatchIndirectBuffer deriving (Eq,Show)
data ShaderStorageBuffer = ShaderStorageBuffer deriving (Eq,Show)

instance BufferTarget ArrayBuffer where
  fromBufferTarget = const GL_ARRAY_BUFFER
instance BufferTarget ElementArrayBuffer where
  fromBufferTarget = const GL_ELEMENT_ARRAY_BUFFER
instance BufferTarget CopyReadBuffer where
  fromBufferTarget = const GL_COPY_READ_BUFFER
instance BufferTarget CopyWriteBuffer where
  fromBufferTarget = const GL_COPY_WRITE_BUFFER
instance BufferTarget PixelUnpackBuffer where
  fromBufferTarget = const GL_PIXEL_UNPACK_BUFFER
instance BufferTarget PixelPackBuffer where
  fromBufferTarget = const GL_PIXEL_PACK_BUFFER
instance BufferTarget QueryBuffer where
  fromBufferTarget = const GL_QUERY_BUFFER
instance BufferTarget TextureBuffer where
  fromBufferTarget = const GL_TEXTURE_BUFFER
instance BufferTarget TransformFeedbackBuffer where
  fromBufferTarget = const GL_TRANSFORM_FEEDBACK_BUFFER
instance BufferTarget UniformBuffer where
  fromBufferTarget = const GL_UNIFORM_BUFFER
instance BufferTarget DrawIndirectBuffer where
  fromBufferTarget = const GL_DRAW_INDIRECT_BUFFER
instance BufferTarget AtomicCounterBuffer where
  fromBufferTarget = const GL_ATOMIC_COUNTER_BUFFER
instance BufferTarget DispatchIndirectBuffer where
  fromBufferTarget = const GL_DISPATCH_INDIRECT_BUFFER
instance BufferTarget ShaderStorageBuffer where
  fromBufferTarget = const GL_SHADER_STORAGE_BUFFER

class BufferMapAccess a where
  fromBufferMapAccess :: a -> GLenum

data MapReadBit = MapReadBit deriving (Eq,Show)
data MapWriteBit = MapWriteBit deriving (Eq,Show)
data MapDynamicStorageBit = MapDynamicStorageBit deriving (Eq,Show)
data MapPersistentBit = MapPersisentBit deriving (Eq,Show)
data MapCoherentBit = MapCoherentBit deriving (Eq,Show)
data MapClientStorage = MapClientStorage deriving (Eq,Show)

instance BufferMapAccess MapReadBit where
  fromBufferMapAccess = const GL_MAP_READ_BIT
instance BufferMapAccess MapWriteBit where
  fromBufferMapAccess = const GL_MAP_WRITE_BIT
instance BufferMapAccess MapDynamicStorageBit where
  fromBufferMapAccess = const GL_DYNAMIC_STORAGE_BIT
instance BufferMapAccess MapPersistentBit where
  fromBufferMapAccess = const GL_MAP_PERSISTENT_BIT
instance BufferMapAccess MapCoherentBit where
  fromBufferMapAccess = const GL_MAP_COHERENT_BIT
instance BufferMapAccess MapClientStorage where
  fromBufferMapAccess = const GL_CLIENT_STORAGE_BIT

genBuffers :: Natural -> GL i i [Buffer]
genBuffers n = wrapGL $ do
  allocaArray (fromIntegral n) $ \p -> do
    glGenBuffers (fromIntegral n) p
    buffers <- peekArray (fromIntegral n) p
    pure $ map Buffer buffers

deleteBuffers :: [Buffer] -> GL i i ()
deleteBuffers buffers = wrapGL $
  withArrayLen (map bufferID buffers) $ glDeleteBuffers . fromIntegral

bindBuffer :: (BufferTarget target,Has (Bound target) i ~ False)
           => Buffer 
           -> target 
           -> GL i (Insert (Bound target) True i) ()
bindBuffer buffer target = wrapGL $
  glBindBuffer (bufferID buffer) (fromBufferTarget target)

bindBufferBase :: (BufferTarget target,Has (Bound target) i ~ False)
               => Buffer
               -> target
               -> Natural
               -> GL i (Insert target True i) ()
bindBufferBase buffer target index = wrapGL $
  glBindBufferBase (bufferID buffer) (fromBufferTarget target) (fromIntegral index)

unbindBuffer :: (BufferTarget target,Has (Bound target) i ~ True)
             => target
             -> GL i (Delete (Bound target) i) ()
unbindBuffer target = wrapGL $ glBindBuffer (fromBufferTarget target) 0

bufferStorage :: (BufferMapAccess mapAccess,BufferTarget target,Has (Bound target) i ~ True,Storable a)
              => target
              -> Natural
              -> [a]
              -> mapAccess 
              -> GL i i ()
bufferStorage target size values mapAccess = wrapGL $
  withArray values $ \p ->
    glBufferStorage (fromBufferTarget target) (fromIntegral size) (castPtr p)
      (fromBufferMapAccess mapAccess)

-- TODO: can’t we track mapped buffers?
mapBufferRange :: (BufferMapAccess mapAccess,BufferTarget target)
               => target
               -> Int
               -> Natural
               -> mapAccess
               -> GL i i (Ptr a)
mapBufferRange target offset size mapAccess = wrapGL . fmap castPtr $ do
  glMapBufferRange (fromBufferTarget target) (fromIntegral offset)
    (fromIntegral size) (fromBufferMapAccess mapAccess)

-- TODO: see mapBufferRange
unmapBuffer :: (BufferTarget target) => target -> GL i i Bool
unmapBuffer = wrapGL . fmap toBool . glUnmapBuffer . fromBufferTarget
