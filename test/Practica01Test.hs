module Main (main) where

import Test.Hspec
import Practica01
import Test.Hspec.Runner

main :: IO ()
main = hspecWith defaultConfig specs


specs :: Spec
specs = do

    describe "Test valorAbs" $ do
        it "Valor absoluto de 15" $ do
            valorAbs 15 `shouldBe` 15
        
        it "Valor absoluto de -25" $ do
            valorAbs (-25) `shouldBe` 25

        it "Valor absoluto de 0" $ do
            valorAbs 0 `shouldBe` 0
    
    describe "Test esDivisor" $ do
        it "Divisor verdadero" $ do
            esDivisor 5 10 `shouldBe` True

        it "Divisor falso" $ do
            esDivisor 3 5 `shouldBe` False

    describe "Tests cuadratica" $ do
        it "Resultado 4" $ do
            cuadratica 1 0 0 2 `shouldBe` 4.0
        
        it "Resultado 7.625" $ do
            cuadratica 0.5 1 2 2.5 `shouldBe` 7.625

        it "Resultado 44.25" $ do
            cuadratica 1 4 18 3.5 `shouldBe` 44.25
        
    describe "Tests sumaFracciones" $ do
        it "Denominador igual" $ do
            sumaFracciones (1,4) (2,4) `shouldBe` (3,4)
        
        it "Denominador distinto" $ do
            sumaFracciones (1,2) (1,4) `shouldBe` (6,8)
 
    describe "Tests comparador" $ do
        it "Iguales" $ do
            comparador 5 5 `shouldBe` 0
        
        it "El primero es mayor" $ do
            comparador 5.5 5 `shouldBe` 1

        it "El segundo es mayor" $ do
            comparador 5.5 9.2 `shouldBe` (-1)
    
    describe "Tests puntoMedio" $ do
        it "Ejemplo" $ do
            puntoMedio (9.2,7) (3, -13.5) `shouldBe` (6.1,-3.25)
        
        it "Otro punto medio" $ do
            puntoMedio (15.67,-23) (84, 107) `shouldBe` (49.835,42.0)
    
    describe "Tests relacionDivisor" $ do
        it "Relacion" $ do
            relacionDivisor `shouldMatchList` [(1,1),(1,3),(1,5),(1,7),(1,9),(1,11),(1,13),(1,15),(1,17),(1,19),(1,21),(1,23),(1,25),(1,27),(1,29),(2,2),(2,4),(2,6),(2,8),(2,10),(2,12),(2,14),(2,16),(2,18),(2,20),(2,22),(2,24),(2,26),(2,28),(2,30),(3,3),(3,9),(3,15),(3,21),(3,27),(4,4),(4,8),(4,12),(4,16),(4,20),(4,24),(4,28),(5,5),(5,15),(5,25),(6,6),(6,12),(6,18),(6,24),(6,30),(7,7),(7,21),(8,8),(8,16),(8,24),(9,9),(9,27),(10,10),(10,20),(10,30),(11,11),(12,12),(12,24),(13,13),(14,14),(14,28),(15,15),(16,16),(17,17),(18,18),(19,19),(20,20),(21,21),(22,22),(23,23),(24,24),(25,25),(26,26),(27,27),(28,28),(29,29),(30,30)]
    
    describe "Tests relacionSumaEspecial" $ do
        it "Relacion" $ do
            relacionSumaEspecial `shouldMatchList` [(1,4),(1,9),(1,14),(1,19),(1,24),(1,29),(2,3),(2,8),(2,13),(2,18),(2,23),(2,28),(3,7),(3,12),(3,17),(3,22),(3,27),(4,6),(4,11),(4,16),(4,21),(4,26),(5,10),(5,15),(5,20),(5,25),(5,30),(6,9),(6,14),(6,19),(6,24),(6,29),(7,8),(7,13),(7,18),(7,23),(7,28),(8,12),(8,17),(8,22),(8,27),(9,11),(9,16),(9,21),(9,26),(10,15),(10,20),(10,25),(10,30),(11,14),(11,19),(11,24),(11,29),(12,13),(12,18),(12,23),(12,28),(13,17),(13,22),(13,27),(14,16),(14,21),(14,26),(15,20),(15,25),(15,30),(16,19),(16,24),(16,29),(17,18),(17,23),(17,28),(18,22),(18,27),(19,21),(19,26),(20,25),(20,30),(21,24),(21,29),(22,23),(22,28),(23,27),(24,26),(25,30),(26,29),(27,28)]

    describe "Tests relacionCongruentesModuloN" $ do
        it "Modulo 5" $ do
            relacionCongruentesModuloN 5 `shouldMatchList` [(1,6),(1,11),(1,16),(1,21),(1,26),(2,7),(2,12),(2,17),(2,22),(2,27),(3,8),(3,13),(3,18),(3,23),(3,28),(4,9),(4,14),(4,19),(4,24),(4,29),(5,10),(5,15),(5,20),(5,25),(5,30),(6,1),(6,11),(6,16),(6,21),(6,26),(7,2),(7,12),(7,17),(7,22),(7,27),(8,3),(8,13),(8,18),(8,23),(8,28),(9,4),(9,14),(9,19),(9,24),(9,29),(10,5),(10,15),(10,20),(10,25),(10,30),(11,1),(11,6),(11,16),(11,21),(11,26),(12,2),(12,7),(12,17),(12,22),(12,27),(13,3),(13,8),(13,18),(13,23),(13,28),(14,4),(14,9),(14,19),(14,24),(14,29),(15,5),(15,10),(15,20),(15,25),(15,30),(16,1),(16,6),(16,11),(16,21),(16,26),(17,2),(17,7),(17,12),(17,22),(17,27),(18,3),(18,8),(18,13),(18,23),(18,28),(19,4),(19,9),(19,14),(19,24),(19,29),(20,5),(20,10),(20,15),(20,25),(20,30),(21,1),(21,6),(21,11),(21,16),(21,26),(22,2),(22,7),(22,12),(22,17),(22,27),(23,3),(23,8),(23,13),(23,18),(23,28),(24,4),(24,9),(24,14),(24,19),(24,29),(25,5),(25,10),(25,15),(25,20),(25,30),(26,1),(26,6),(26,11),(26,16),(26,21),(27,2),(27,7),(27,12),(27,17),(27,22),(28,3),(28,8),(28,13),(28,18),(28,23),(29,4),(29,9),(29,14),(29,19),(29,24),(30,5),(30,10),(30,15),(30,20),(30,25)]
        
        it "Modulo 17" $ do
            relacionCongruentesModuloN 17 `shouldMatchList` [(1,18),(2,19),(3,20),(4,21),(5,22),(6,23),(7,24),(8,25),(9,26),(10,27),(11,28),(12,29),(13,30),(18,1),(19,2),(20,3),(21,4),(22,5),(23,6),(24,7),(25,8),(26,9),(27,10),(28,11),(29,12),(30,13)]

    describe "Tests esPar" $ do
        it "Impar" $ do
            esPar (Suc (Suc (Suc Cero))) `shouldBe` False
        
        it "Par" $ do
            esPar (Suc (Suc (Suc (Suc Cero)))) `shouldBe` True

    describe "Test iguales" $ do
        it "iguales" $ do
            iguales (Suc (Suc (Suc Cero))) (Suc (Suc (Suc Cero))) `shouldBe` True
        it "distintos" $ do
            iguales (Suc (Suc (Suc Cero))) (Suc (Suc Cero)) `shouldBe` False

    describe "Test maximo" $ do
        it "El primero es mayor" $ do
            maximo (Suc (Suc (Suc Cero))) (Suc (Suc Cero)) `shouldBe` (Suc (Suc (Suc Cero)))
        it "El segundo es mayor" $ do
            maximo (Suc (Suc (Suc Cero))) (Suc (Suc (Suc (Suc Cero)))) `shouldBe` (Suc (Suc (Suc (Suc Cero))))
    
    describe "Test potencia" $ do
        it "2 a la 3" $ do
            potencia (Suc (Suc Cero)) (Suc (Suc (Suc Cero))) `shouldBe` (Suc (Suc (Suc (Suc (Suc (Suc (Suc (Suc Cero))))))))
        it "3 a la 2" $ do
            potencia (Suc (Suc (Suc Cero))) (Suc (Suc Cero)) `shouldBe` (Suc (Suc (Suc (Suc (Suc (Suc (Suc (Suc (Suc Cero)))))))))
