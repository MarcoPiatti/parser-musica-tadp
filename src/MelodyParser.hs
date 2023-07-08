module MelodyParser (
  melodia,
  tocable,
  tono,
  figura,
  nota
) where

import Melody
import Parser as P

import Control.Applicative ( Alternative((<|>)) )
import Data.Function ( (&) )
import Data.Maybe ( fromMaybe )

melodia :: Parser Melodia
melodia = Melodia <$> sepBy tocable (char ' ')

tocable :: Parser Tocable
tocable = silencio <|> sonido <|> acorde

silencio :: Parser Tocable
silencio =  Silencio Blanca <$ char '_' <|> 
            Silencio Negra <$ char '-' <|> 
            Silencio Corchea <$ char '~' 

sonido :: Parser Tocable 
sonido = Sonido <$> tono <*> figura

tono :: Parser Tono
tono = Tono <$> integer <*> nota

integer :: Parser Int
integer = read <$> (P.+) digit

nota :: Parser Nota
nota = ((&) <$> notaBase) <*> (fromMaybe id <$> opt modificador)

notaBase :: Parser Nota
notaBase =  C <$ char 'C' <|>
            D <$ char 'D' <|>
            E <$ char 'E' <|>
            F <$ char 'F' <|>
            G <$ char 'G' <|>
            A <$ char 'A' <|>
            B <$ char 'B'

modificador :: Parser (Nota -> Nota)
modificador = sostenido <$ char '#' <|> bemol <$ char 'b'

figura :: Parser Figura
figura =  SemiCorchea <$ string "1/16" <|> 
          Corchea <$ string "1/8" <|>
          Blanca <$ string "1/2" <|>
          Negra <$ string "1/4" <|>
          Redonda <$ string "1/1"

acorde :: Parser Tocable
acorde = (acordeImplicito <|> acordeExplicito) <*> figura

acordeExplicito :: Parser (Figura -> Tocable)
acordeExplicito = Acorde <$> sepBy tono (char '+')

acordeImplicito :: Parser (Figura -> Tocable)
acordeImplicito = ((&) <$> tono) <*> tipoAcorde

tipoAcorde :: Parser (Tono -> Figura -> Tocable)
tipoAcorde =  acordeMenor <$ char 'm' <|>
              acordeMayor <$ char 'M'