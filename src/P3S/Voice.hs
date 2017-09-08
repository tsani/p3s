module P3S.Voice where

import P3S.Apply
import P3S.Generator
import P3S.Position
import P3S.Math

data Line i n d
  = Line
    { lineInstrument :: !i
    , lineNote :: !n
    , lineDuration :: !d
    }

-- | Constructs a line of the given duration with no note and no instrument by
-- using @mempty@ from the monoid instances.
silence :: (Monoid i, Monoid n) => d -> Line i n d
silence d =
  Line { lineInstrument = mempty, lineNote = mempty, lineDuration = d }

-- | A voice is a sequence of notes paired with an instrument and a duration.
-- Multiple voices can be overlayed to create harmony.
newtype Voice i n d = Voice { unVoice :: [Line i n d] }

-- | Voices are combined sequentially.
instance Monoid (Voice i n d) where
  mempty = Voice []
  mappend (Voice v1) (Voice v2) = Voice (v1 <> v2)

-- | Measures the nominal duration of a voice.
--
-- Note that generators can produce sound even after their advertised durations
-- have elapsed. This is the difference between /nominal/ and /real/ duration.
duration :: Monoid d => Voice i n d -> d
duration = mconcat . map lineDuration . unVoice

-- | Compile a voice of generators into a single position.
compile
  :: forall n t a. (Ord t, Group t, Monoid t, Monoid a)
  => Voice (Generator n t a) n t -- ^ voice to compile
  -> Position t a -- ^ overall audio
compile (Voice ls) = mconcat $ go mempty ls where
  go :: t -> [Line (Generator n t a) n t] -> [Position t a]
  go _ [] = []
  go t (Line g n d : ls') = offset t (g @@ n) : go (t <> d) ls'

-- play
--   :: forall n t a. (Monoid a, Monoid n, Ord t)
--   => Voice (Generator n t a) n t -> Generator n t a
-- play (Voice ls) = go ls where
--   go :: [Line (Generator n t a) n t] -> Generator n t a
