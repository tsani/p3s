module Main where

import qualified Data.ByteString.Lazy as LBS
import Data.Monoid ( (<>) )

import P3S.Audio

env = ADSR
  { attack = ms 50
  , decay = mempty -- ms 50
  , sustain = Intensity 1
  , release = ms 200
  }

main :: IO ()
main = do
  let rate = hz44100
  let w = period rate

  let sq = vol' 0.1 $ scale <$> (constant $ adsr env mempty) <*> square
  let as = sampleFor mempty w (Hz 440) sq (sec 2)

  let output = as

  LBS.putStr $ renderWith s16le output
