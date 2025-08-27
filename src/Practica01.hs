module Practica01 where

--FUNCIONES
valorAbs :: Int -> Int
valorAbs = undefined

esDivisor :: Int -> Int -> Bool
esDivisor = undefined 

cuadratica :: Float -> Float -> Float -> Float -> Float
cuadratica = undefined


sumaFracciones :: (Int, Int) -> (Int, Int) -> (Int, Int)
sumaFracciones = undefined

comparador :: Float -> Float -> Int
comparador = undefined

puntoMedio :: (Float, Float) -> (Float, Float) -> (Float, Float)
puntoMedio = undefined


--RELACIONES
type Rel a b = [(a, b)]

relacionDivisor :: Rel Int Int
relacionDivisor = undefined

relacionSumaEspecial :: Rel Int Int
relacionSumaEspecial = undefined

relacionCongruentesModuloN :: Int -> Rel Int Int
relacionCongruentesModuloN = undefined


--NATURALES
-- Cero es natural, Suc Cero es natural, Suc Suc Cero es natural, etc.
data Natural = Cero | Suc Natural deriving (Show,Eq) --Esto es para que se muestre y que se puedan comparar

esPar :: Natural -> Bool
esPar = undefined

iguales :: Natural -> Natural -> Bool
iguales = undefined

maximo :: Natural -> Natural -> Natural 
maximo = undefined

potencia :: Natural -> Natural -> Natural
potencia = undefined

multiplicacion :: Natural -> Natural -> Natural
multiplicacion = undefined

suma :: Natural -> Natural -> Natural
suma = undefined
