module P3S.PCM where

import P3S.Physics

import Data.Binary.Builder
import Data.ByteString ( ByteString )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Int
import Data.Monoid ( (<>) )
import Data.Word

-- | Pulse code modulated audio data.
data PCM
  = PCM
    { pcmData :: LBS.ByteString
    , pcmSampleInfo :: SampleInfo
    }

type SampleRenderer = Amplitude -> Builder

-- | Information about samples in PCM audio data.
data SampleInfo
  = SampleInfo
    { sampleInfoRate :: Hz
    , sampleRenderer :: SampleRenderer
    }

s16le :: SampleRenderer
s16le a = putInt16le (rescaledIntegral a)

rescaledIntegral :: forall i. (Bounded i, Integral i) => Amplitude -> i
rescaledIntegral (A a) = floor (a' * w + mi) where
  a' = (a + 1) / 2
  mi = fromIntegral (minBound :: i)
  ma = fromIntegral (maxBound :: i)
  w = ma - mi
  -- width of the bounded space we're mapping to

data SampleType
  = IntSample Signedness
  | FloatSample

data Signedness
  = Unsigned
  | Signed

-- | The byte width of a sample.
data SampleWidth
  = SingleWidth
  | DoubleWidth

hz44100 :: Hz
hz44100 = Hz 44100

renderWith
  :: SampleRenderer -- ^ Sample renderer
  -> [Amplitude] -- ^ Samples to render
  -> LBS.ByteString -- ^ PCM data.
renderWith f as = toLazyByteString $ mconcat (f <$> as)
