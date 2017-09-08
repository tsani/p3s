module P3S.Transformer.Volume where

import P3S.Apply
import P3S.Generator
import P3S.Math
import P3S.Misc
import P3S.Physics
import P3S.Position

import Data.Function ( on )
import Data.Coerce

-- | Scale down the volume.
vol :: Functor f => Intensity -> f Amplitude -> f Amplitude
vol (Intensity v) = fmap (coerce . (v *) . coerce)

-- | Scale down the volume of a generator.
--
-- /Calls @error@ if the given @Double@ is out of bounds./
vol' :: Functor f => Double -> f Amplitude -> f Amplitude
vol' = vol . unsafeIntensity

-- | An ADSR envelope.
data ADSR t
  = ADSR
    { attack :: t
    , decay :: t
    , sustain :: Intensity
    , release :: t
    }

-- | Computes the time spent applying the ADSR envelope.
--
-- If a sound has a total play time of 1 second and the ADSR time is 0.1
-- seconds, then the sustain time is 0.9 seconds.
adsrTime :: Monoid t => ADSR t -> t
adsrTime ADSR{..} = attack <> decay <> release

adsr'
  :: (Group t, Ord t, Between t, Progress t ~ Intensity)
  => ADSR t -- ^ The ADSR envelope to apply.
  -> t -- ^ The total desired play time of the sound.
  -> Position t Intensity -- ^ A generator for intensity levels.
adsr' env tot = adsr env (diff tot at) where
  at = adsrTime env

adsr
  :: forall t. (Monoid t, Ord t, Between t, Progress t ~ Intensity)
  => ADSR t -- ^ The ADSR envelope to apply.
  -> t -- ^ The time to sustain.
  -> Position t Intensity -- ^ A generator for intensity levels.
adsr ADSR{..} s = Position go where
    a = attack
    d = decay
    (Intensity si) = sustain
    r = release

    go :: t -> Intensity
    go t
      -- intensity before time zero is also zero
      | mempty > t = mempty
      -- rise from 0 to 1 according to the attack time.
      | mempty <= t && t < a = between mempty a t
      -- after attack, decay to sustain intensity over decay time.
      | a <= t && t < a <> d =
        let (Intensity i) = between a (a <> d) t
        in invert (Intensity $ si * i)
      -- after decay, remain at sustain intensity over sustain time.
      | a <> d <= t && t < a <> d <> s = Intensity si
      -- after sustain, decay from sustain intensity to zero over release time.
      | a <> d <> s <= t && t < a <> d <> s <> r =
        let (Intensity i) = between (a <> d <> s) (a <> d <> s <> r) t
        in Intensity (si - si * i)
      -- after release, intensity is zero.
      | t >= (a <> d <> s <> r) = mempty
