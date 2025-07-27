module NetworkUtils (encode16BE, decode16BE) where

import Control.Exception
import Data.Bits
import Data.Word
import GHC.Num

-- | Interpret the given value as an unsigned 16-bit integer and encode it in big endian representation for the network.
encode16BE :: Integer -> [Word8]
encode16BE x = [msb, lsb]
  where
    lsb = fromInteger $ x .&. 255
    msb = fromInteger $ x `shiftR` 255

-- | Decode a given byte array into an unsigned 16-bit integer. Throws when the input isn't an array of length 2.
decode16BE :: [Word8] -> IO Int
decode16BE [msb, lsb] = return $ (msb' `shiftL` 8) .|. lsb'
  where
    msb' = integerToInt $ toInteger msb
    lsb' = integerToInt $ toInteger lsb
decode16BE _ = throw $ UndefinedElement "Length read impossible."
