-- =====================================================================
--  CI-3661 — Tarea 1 (Haskell) — Septiembre–Diciembre 2026
--
--  Nombre: Alejandro Zambrano
--  Carnet: 17-10684
--
--  Declaración de uso de herramientas de IA (obligatoria, ver enunciado):
--  Gemini para preguntas teoricas sobre Haskell y etender la sintaxis y
--  Copilot para explicaciones de errores, preguntas sobre compilación.
-- =====================================================================
--  No cambies el nombre del módulo ni la lista de exportación, ni las
--  firmas de las funciones. Puedes agregar todas las funciones auxiliares
--  que quieras. El único import permitido es Data.Char.
-- =====================================================================
module Tarea1
  ( esPalindromo
  , mayorYMenor
  , leerHora
  , duracion
  , promedioSobreUmbral
  , movimientosCaballo
  , enDosSaltos
  , miMap
  , miFilter
  , miFoldl
  ) where

import Data.Char (isAlphaNum, isDigit, toLower, digitToInt)

-- =====================================================================
-- Parte A: recursión explícita.
-- Prohibido: map, filter, foldr, foldl, listas por comprensión, reverse,
-- maximum, minimum, all, any y cualquier otra función de orden superior.
-- =====================================================================


-- A1
esPalindromo :: String -> Bool
esPalindromo [] = True
esPalindromo xs = esPalindromoAux xs xs 0 (length xs)

esPalindromoAux :: String -> String -> Int -> Int -> Bool
esPalindromoAux xs ys i j
  | i >= j = True
  | not (isAlphaNum (xs !! i)) = esPalindromoAux xs ys (i + 1) j
  | not (isAlphaNum (ys !! (j - 1))) = esPalindromoAux xs ys i (j - 1)
  | toLower (xs !! i) /= toLower (ys !! (j - 1)) = False
  | otherwise = esPalindromoAux xs ys (i + 1) (j - 1)

-- A2
-- Explica aquí, en dos o tres líneas, por qué tu solución recorre la
-- lista una sola vez: como usamos head y tail para recorrer la lista ((x:xs)),
-- esta se recorre una sola vez, y al almacenar el máximo y mínimo en una tupla,
-- no necesitamos recorrer la lista nuevamente para obtenerlos y comprar con la head actual (x).
mayorYMenor :: Ord a => [a] -> Maybe (a, a)
mayorYMenor [] = Nothing
mayorYMenor (x:xs) = mayorYMenorAux xs (x, x)

mayorYMenorAux :: Ord a => [a] -> (a, a) -> Maybe (a, a)
mayorYMenorAux [] (max, min) = Just (max, min)
mayorYMenorAux (x:xs) (max, min)
  | x > max = mayorYMenorAux xs (x, min)
  | x < min = mayorYMenorAux xs (max, x)
  | otherwise = mayorYMenorAux xs (max, min)


-- A3
leerHora :: String -> Either String (Int, Int)
leerHora = undefined

duracion :: String -> String -> Either String Int
duracion = undefined

-- =====================================================================
-- Parte B: orden superior.
-- Prohibido: recursión explícita (ninguna función de esta parte, ni sus
-- auxiliares, puede llamarse a sí misma).
-- =====================================================================

-- B1
promedioSobreUmbral :: Double -> [Double] -> Maybe Double
promedioSobreUmbral = undefined

-- B2
movimientosCaballo :: (Int, Int) -> [(Int, Int)]
movimientosCaballo = undefined

enDosSaltos :: (Int, Int) -> Int
enDosSaltos = undefined

-- B3
miMap :: (a -> b) -> [a] -> [b]
miMap = undefined

miFilter :: (a -> Bool) -> [a] -> [a]
miFilter = undefined

miFoldl :: (b -> a -> b) -> b -> [a] -> b
miFoldl = undefined
