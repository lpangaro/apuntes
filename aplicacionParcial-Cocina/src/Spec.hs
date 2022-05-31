module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "Test de ejemplo" $ do
    it "El pdepreludat se instaló correctamente" $ do
      doble 1 `shouldBe` 2
  describe "Test de multiplicaciones" $ do
    it "el triple de un numero" $ do
      triple 2 `shouldBe` 6
  describe "Test de cocina" $ do
   it "la plancha mejor el sabor del choclo" $ do
      sabor (plancha choclo) `shouldBe` 60
   it "un plato a la plancha con solo choclo y calabaza no es un buen plato" $ do
      buenPlatoALaPlancha [choclo,cala] `shouldBe` False
   it "un plato a la plancha con choclo, calabaza y carne es un buen plato" $ do
      buenPlatoALaPlancha algunosAlimentos `shouldBe` True
   it "un plato a la plancha con choclo, calabaza y carne es un buen plato" $ do
      buenPlato plancha algunosAlimentos `shouldBe` True
   it "un plato a la proveletera con choclo, calabaza es un buen plato" $ do
      buenPlato provoletera [choclo,cala] `shouldBe` True
   it "un plato frito con choclo, calabaza es un buen plato" $ do
      buenPlato (olla "aceite") [choclo,cala] `shouldBe` False
   it "un plato frito con choclo, calabaza y carne es un buen plato" $ do
      buenPlato (olla "aceite") algunosAlimentos `shouldBe` True
   it "un plato herbido con choclo, calabaza no es un buen plato" $ do
      buenPlato (olla "agua") [choclo,cala] `shouldBe` False
