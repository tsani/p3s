module P3S.Note where

-- | Names of the difference keys on a keyboard.
data KeyName
  = KA
  | KBbA'
  | KB
  | KC
  | KDbC'
  | KD
  | KEbD'
  | KE
  | KF
  | KGbF'
  | KG
  | KAbG'
  deriving (Bounded, Enum, Eq, Read, Show)

data Diatonic =
  Diatonic
    { noteName :: NoteName
    , alteration :: Int
    }


-- | A note pitch is a frequency expressed relative to a fixed base frequency
-- by a number of semitones.
newtype NotePitch = NotePitch { note :: Interval }

-- | Computes the interval distance between two notes.
diffNote :: Note -> Note -> Interval
diffNote (Note n1) (Note n2) = n1 `diff` n2

-- | An interval is a number of semitones between two notes.
newtype Interval = I Int

instance Monoid Interval where
  mempty = I 0
  mappend (I i1) (I i2) = I (i1 + i2)

instance Group Inverval where
  inv (I i) = I (negate i)
  diff (I i1) (I i2) = I (i1 - i2)

minorThird = I 3
majorThird = I 4
perfectFourth = I 5
perfectFifth = I 7
minorSixth = I 8
majorSixth = I 9

n :: NoteName -> Octave -> Note
n (N nn) (O o) = Note (nn + o)

up :: Interval -> Note -> Note
up (I i') (Note (I i)) = Note (I (i + i'))

down :: Interval -> Note -> Note
down i = up (invert i)

invert :: Interval -> Interval
invert (I i) = (I (negate i))

newtype Octave = O { octave :: Int }
newtype NoteName = N { noteName :: Int }

sharp :: Note -> Note
sharp (Note (I n)) = Note (I $ n + 1)

flat :: Note -> Note
flat (Note (I n)) = Note (I $ n - 1)

o0 = O -48
o1 = O -36
o2 = O -24
o3 = O -12
o4 = O 0
o5 = O 12
o6 = O 24
o7 = O 36
o8 = O 48

a = N 0
b = N 2
g = N -2
f = N -4
e = N -5
d = N -7
c = N -9
