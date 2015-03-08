------------------------------------------------------------------------------
-- | A light-weight wrapper around @Network.Wai@ to provide easy io-streams support.
module System.IO.Streams.Wai
    ( -- * Types
        Flush(..)
      -- * Request body
      , inputStreamBody
      -- * Response body
      , responseInputStream
      , responseRawInputStream
      -- * Re-export
    , module Network.Wai
    ) where

------------------------------------------------------------------------------
import           Blaze.ByteString.Builder (Builder)

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Bool
import           Data.ByteString ( ByteString )
import qualified Data.ByteString as S
import qualified Data.Foldable as F
import           Network.HTTP.Types
import           Network.Wai
import qualified System.IO.Streams as Streams
import           System.IO.Streams
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- | Flush
data Flush a = Chunk a | Flush
  deriving (Eq, Ord, Show)

------------------------------------------------------------------------------
-- | Functor Flush
instance Functor Flush where
  fmap f c = case c of
    Chunk a -> Chunk $ f a
    Flush -> Flush

------------------------------------------------------------------------------
-- | Stream the request body.
inputStreamBody :: MonadIO m => Request -> m (InputStream ByteString)
inputStreamBody request = 
  liftIO $ makeInputStream $ do
    bs <- requestBody request
    return $ bool Nothing (Just bs) (S.null bs)

------------------------------------------------------------------------------
-- | Create an HTTP response out of an @InputStream@.
responseInputStream
 :: Status
 -> ResponseHeaders
 -> IO (InputStream (Flush Builder))
 -> Response
responseInputStream status headers stream = 
  responseStream status headers $ \send flush -> do
    is <- stream
    os <- makeOutputStream $ \m ->
            F.forM_ m $ \x ->
                case x of
                  Chunk b -> send b
                  Flush   -> flush
    Streams.supply is os


------------------------------------------------------------------------------
-- | Create a raw response using a @InputStream@ and an @OutputStream@ to represent the input
-- and output, respectively.
responseRawInputStream
  :: (IO (InputStream ByteString) -> IO (OutputStream ByteString) -> IO ())
  -> Response
  -> Response
responseRawInputStream app = responseRaw f
  where
    f recv send = app src sink
      where
        src  = makeInputStream $ do
                 bs <- recv
                 return $ bool Nothing (Just bs) (S.null bs)
        sink = Streams.nullOutput >>= Streams.contramapM send


