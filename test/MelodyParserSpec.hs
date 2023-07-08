module MelodyParserSpec (spec) where

import Parser ( Parser(runParser) ) 
import MelodyParser
import Melody 
import Test.Hspec ( describe, it, shouldBe, context, Spec )

felizCumpleanios :: Melodia
felizCumpleanios = Melodia 
  [ Sonido (Tono 4 C) Negra
  , Sonido (Tono 4 C) Negra
  , Sonido (Tono 4 D) Blanca
  , Sonido (Tono 4 C) Negra
  , Sonido (Tono 4 F) Blanca
  , Sonido (Tono 4 E) Blanca 
  ]


spec :: Spec
spec = do
  describe "parser de notas" $ do
    it "parsea una unica nota" $ do
      runParser nota "C" `shouldBe` Right (C, "")
    it "parsea una nota sostenida/disminuida" $ do
      runParser nota "C#" `shouldBe` Right (Cs, "")
      runParser nota "Cb" `shouldBe` Right (B, "")

  describe "parser de figuras" $ do
    it "parsea una fraccion 1/X a una figura" $ do
      runParser figura "1/1" `shouldBe` Right (Redonda, "")
      runParser figura "1/2" `shouldBe` Right (Blanca, "")
      runParser figura "1/4" `shouldBe` Right (Negra, "")
      runParser figura "1/8" `shouldBe` Right (Corchea, "")
      runParser figura "1/16" `shouldBe` Right (SemiCorchea, "")

  describe "parser de tonos" $ do
    it "parsea un tono (una octava con una nota)" $ do
      runParser tono "5F#" `shouldBe` Right (Tono 5 Fs, "")
      runParser tono "12Gb" `shouldBe` Right (Tono 12 Fs, "")

  describe "parser de tocables" $ do
    it "parsea un sonido (tono seguido de figura)" $ do
      runParser tocable "4A#1/4" `shouldBe` Right (Sonido (Tono 4 As) Negra, "")
    it "parsea un silencio con duracion de una figura" $ do
      runParser tocable "_" `shouldBe` Right (Silencio Blanca, "")
      runParser tocable "-" `shouldBe` Right (Silencio Negra, "")
      runParser tocable "~" `shouldBe` Right (Silencio Corchea, "")
    it "parsea un acorde explicito" $ do
      runParser tocable "6A+6C#+6G1/8" `shouldBe` Right (Acorde [Tono 6 A, Tono 6 Cs, Tono 6 G] Corchea, "")
    it "parsea un acorde implicito" $ do
      runParser tocable "6AM1/2" `shouldBe` Right (Acorde [Tono 6 A, Tono 7 Cs, Tono 7 E] Blanca, "")
  
  describe "parser de melodia" $ do
    it "parsea una secuencia de tocables" $ do
      runParser melodia "4C1/4 4C1/4 4D1/2 4C1/4 4F1/2 4E1/2" `shouldBe` Right (felizCumpleanios, "")