module Fitba.Hash (md5sum) where

import qualified Data.ByteString as BS
import Crypto.Hash

md5sum :: BS.ByteString -> BS.ByteString
md5sum = digestToHexByteString . md5
    where
        md5 :: BS.ByteString -> Digest MD5
        md5 = hash
