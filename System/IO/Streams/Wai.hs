-- | A light-weight wrapper around @Network.Wai@ to provide easy io-streams support.
module System.IO.Streams.Wai
    ( Flush(..)
      -- * Request body
    , inputStreamRequestBody
      -- * Response body
    , responseInputStream
    , responseRawInputStream
      -- * Re-export
    , module Network.Wai
    ) where

import Data.Bool
import Blaze.ByteString.Builder (Builder)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import Data.IORef
import qualified Data.ByteString as S

import           System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as Streams
import Network.HTTP.Types
import Network.Wai
import System.IO.Streams

data Flush a = Chunk a | Flush
             deriving (Eq, Ord, Show)

instance Functor Flush where
  fmap f c = case c of
    Chunk a -> Chunk $ f a
    Flush -> Flush

-- | Stream the request body.
--
-- Since 3.0.0
-- inputStreamRequestBody :: MonadIO m => Request -> m (InputStream ByteString)
-- inputStreamRequestBody req = 
--   makeInputStream $ do
--     bs <- liftIO (requestBody req)
--     return $ bool Nothing (Just bs) (S.null bs)

-- | Create an HTTP response out of a @Producer@.
--
-- Since 3.0.0
-- responseProducer :: Status -> ResponseHeaders -> Producer (Flush Builder) IO () -> Response
-- responseProducer s hs src = responseStream s hs $ \send flush ->
--   runEffect $ for src $ \mbuilder -> case mbuilder of
--     Chunk b -> lift $ send b
--     Flush -> lift $ flush

-- | Create a raw response using a @Producer@ and @Consumer@ to represent the input
-- and output, respectively.
--
-- Since 3.0.0
responseRawProducer
  :: (MonadIO m, MonadIO n)
  => (m (InputStream ByteString) -> n (OutputStream ByteString) -> IO ())
  -> Response
  -> Response
responseRawProducer app = responseRaw app'
  where
    app' recv send = app src sink
      where
        src = liftIO recv >>= Streams.fromByteString
        sink bs =
          if S.null bs 
            then makeOutputStream Nothing
            else makeOutputStream (Just bs)
