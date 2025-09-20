module Practica01 where

--Funcion de valor absoluto (1)
  --Si x es menor que 0 , devolvemos -x (cambaindo de signo) y si no devolvemos x

valorAbs :: Int -> Int
valorAbs x = if x < 0 then -x else x
   
  
--Funcion divisor (2)

{-
 Ya que no podemos utilizar las valiables "mod" ni "div" para realizar la division de dos enteros, utiizaremos el metodo por multiplicacion
 -probar valores de k (desde 0 en adelante).
 -Si |a * k| sobrepasa |b|, ya no puede haber divisor.
-}

esDivisor :: Int -> Int -> Bool
esDivisor a b
    | a == 0    = False
    | otherwise = existek 0
  where
    existek k                    
        | a * k == b = True
        | abs (a * k) > abs b = False
        | otherwise = existek (k + 1)

--Funcion cuadratica (3)
  -- Evalúa la expresión ax^2 + bx + c en el valor v

cuadratica :: Float -> Float -> Float -> Float -> Float
cuadratica a b c v = a * v^2 + b * v + c

--Suma de Fracciones (4)

--Suma dos fracciones representadas como tuplas (num, den)

sumaFracciones :: (Int, Int) -> (Int, Int) -> (Int, Int)
sumaFracciones (n1, d1) (n2, d2) =
    if d1 == d2
        then (n1 + n2, d1)                          -- mismo denominador
        else (n1 * d2 + n2 * d1, d1 * d2)           -- distinto denominador

{-
 La prueba se comprobo con los ejemplos definidos en la practica por parte del profesor.
 -Para poder correr el ejemplos es importante poner la variable al inicio y el ejemplo que querramos probrar despues de este

 Ejemplos de prueba en GHCI:
 valorAbs 5            ==> 5
 valorAbs (-10)        ==> 10
 esDivisor 5 10        ==> True
 esDivisor 3 5         ==> False
 cuadratica 1 0 0 2    ==> 4.0
 cuadratica 0.5 1 2 2.5 ==> 7.625
 sumaFracciones (1,4) (2,4) ==> (3,4)
 sumaFracciones (1,2) (1,4) ==> (6,8)
-}
--La función debe recibir dos números n, m. Se debe devolver 0 si n = m, 1 si n > m y, −1 si m > n
comparador :: Float -> Float -> Int
comparador n m
    | n == m = 0
    | n > m = 1
    | m > n = -1
--Punto medio. La función debe recibir dos puntos del plano cartesiano y devolver el punto medio entre ellos.
puntoMedio :: (Float,Float) -> (Float,Float) -> (Float,Float)
puntoMedio (x1, y1) (x2, y2) = ((x1 + x2) / 2 , (y1 + y2) / 2)


--RELACIONES
type Rel a b = [(a, b)]
--Relacion divisor. En esta relación R1, tenemos que aR1b si a y b tienen la misma paridad y a es divisor de b.
pares :: Int -> [Int]
pares n = [x | x <- [1..30], esMultiplo x 2]
impares :: Int -> [Int]
impares n = [x | x <- [1..30], not (esMultiplo x 2)]
esDivisor :: Int -> Int -> Bool
esDivisor a b
    | a == 0    = False
    | otherwise = existek 0
  where
    existek k                    
        | a * k == b = True
        | abs (a * k) > abs b = False
        | otherwise = existek (k + 1)

relacionDivisor :: Rel Int Int
relacionDivisor = [(x,y) | x <- pares 30 , y <- pares 30, esDivisor x y]++[(x,y) | x <- impares 30 , y <- impares 30, esDivisor x y]

--Relacion suma especial. En esta relación R2, tenemos que aR2b si a + b es múltiplo de 5 y a < b.
suma :: Int -> Int -> Int
suma n m = n + m
menorQue :: Int -> Int -> Bool
menorQue x y = x < y
relacionSumaEspecial :: Rel Int Int
relacionSumaEspecial = [(x,y) | x <-[1..30] , y <- [1..30], esMultiplo (suma x y) 5 && menorQue x y]
--Relacion congruentes modulo N. En esta relación R3, se debe recibir un entero n y tenemos que aR3b si a %n = b %n con a ̸= b.
numerosIguales :: Int -> Int -> Bool
numerosIguales x y
               |x == y = True
               | otherwise = False
numerosDiferentes :: Int -> Int -> Bool
numerosDiferentes n m
               |n == m = False
               | otherwise = True
residuos :: Int -> Int -> Int -> Bool
residuos n m b = (n `mod` b) == (m `mod` b)
relacionCongruentesModuloN :: Rel Int Int
relacionCongruentesModuloN = [(x,y) | x <- [1..30] , y <- [1..30], residuos x y 5, numerosDiferentes x y]



--NATURALES
-- Cero es natural, Suc Cero es natural, Suc Suc Cero es natural, etc.
data Natural = Cero | Suc Natural deriving (Show,Eq) --Esto es para que se muestre y que se puedan comparar

--La función debe recibir un número natural y determinar si es par o no
esPar :: Natural -> Bool
esPar Cero = True
esPar (Suc Cero) = False
esPar (Suc (Suc n)) = esPar n

--La función debe recibir dos números naturales y determinar si estos números son iguales o no
iguales :: Natural -> Natural -> Bool
iguales Cero Cero = True
iguales Cero (Suc n) = False
iguales (Suc n) Cero = False
iguales (Suc n) (Suc m) = iguales n m

--La función debe recibir dos números naturales y devolver el que sea mayor
maximo :: Natural -> Natural -> Natural 
maximo Cero n = n
maximo n  Cero = n
maximo (Suc n) (Suc m) = Suc (maximo n m)

--La función debe recibir dos números naturales n, m y devolver el resultado de n^m
potencia :: Natural -> Natural -> Natural
potencia _ Cero = Suc Cero   --n⁰ = 1
potencia n (Suc m) = multiplicacion n (potencia n m)  --

multiplicacion :: Natural -> Natural -> Natural
multiplicacion Cero m = Cero
multiplicacion (Suc Cero) m = m
multiplicacion (Suc n) m = suma m (multiplicacion n m)

suma :: Natural -> Natural -> Natural
suma Cero m = m
suma (Suc n) m = Suc (suma n m)
