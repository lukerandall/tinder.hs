module Web.Tinder.Util
    ( toStrict
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

-- TODO: remove this when we upgrade bytestring
-- toStrict = BL.toStrict

toStrict :: BL.ByteString -> B.ByteString
toStrict = B.concat . BL.toChunks
