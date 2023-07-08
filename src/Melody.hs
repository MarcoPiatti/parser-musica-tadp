module Melody (
  Melodia(..),
  Tocable(..),
  Tono(..),
  Figura(..),
  Nota(..),
  sostenido,
  bemol,
  acordeMenor,
  acordeMayor
) where

data Nota = C | Cs | D | Ds | E | F | Fs | G | Gs | A | As | B deriving (Show, Enum, Eq)
data Figura = Redonda | Blanca | Negra | Corchea | SemiCorchea deriving (Show , Eq)
data Tono = Tono Int Nota deriving (Show , Eq)
data Tocable = Sonido Tono Figura | Silencio Figura | Acorde [Tono] Figura deriving (Show , Eq)
newtype Melodia = Melodia [Tocable] deriving (Show , Eq)

sostenido :: Nota -> Nota
sostenido B = C
sostenido nota = succ nota

bemol :: Nota -> Nota
bemol C = B
bemol nota = pred nota

shiftTono :: Int -> Tono -> Tono
shiftTono x (Tono oct nota) = Tono (oct + (fromEnum nota + x) `div` 12) $ toEnum ((fromEnum nota + x) `mod` 12)

acordeMenor :: Tono -> Figura -> Tocable
acordeMenor t = Acorde [t, shiftTono 3 t, shiftTono 7 t]

acordeMayor :: Tono -> Figura -> Tocable
acordeMayor t = Acorde [t, shiftTono 4 t, shiftTono 7 t]