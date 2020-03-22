module Data.Digest.CRC32 (digest, update) where

import Data.ByteString (ByteString(..))
import qualified Data.ByteString as BS
import Data.Word
import Data.List (foldl')
import Data.Bits
import Data.Array

digest :: ByteString -> Word32
digest = update 0

update :: Word32 -> ByteString -> Word32
update v bytes = xor maxBound $ BS.foldl' updateStep (v `xor` maxBound) bytes

updateStep :: Word32 -> Word8 -> Word32
updateStep previous byte = (previous `shiftR` 8) `xor` (crcUpdateTable ! ((fromIntegral $ previous .&. 0xFF) `xor` byte))

crcUpdateTable :: Array Word8 Word32
crcUpdateTable = listArray (minBound, maxBound) [foldl' (\a _ -> shiftR a 1 `xor` if testBit a 0 then 0xedb88320 else 0) (fromIntegral i) [0..7] | i <- [minBound :: Word8 .. maxBound]]
