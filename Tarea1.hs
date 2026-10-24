-- =====================================================================
--  CI-3661 — Tarea 1 (Haskell) — Septiembre–Diciembre 2026
--
--  Nombre: Alejandro Zambrano
--  Carnet: 17-10684
--
--  Declaración de uso de herramientas de IA (obligatoria, ver enunciado):
--  Gemini para preguntas teoricas sobre Haskell y etender la sintaxis y
--  Copilot para explicaciones de errores y preguntas sobre compilación.
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
-- no necesitamos recorrer la lista nuevamente para obtenerlos y comparar con la head actual (x).
mayorYMenor :: Ord a => [a] -> Maybe (a, a)
mayorYMenor [] = Nothing
mayorYMenor (x:xs) = mayorYMenorAux xs (x, x)

mayorYMenorAux :: Ord a => [a] -> (a, a) -> Maybe (a, a)
mayorYMenorAux [] (maximo, minimo) = Just (maximo, minimo)
mayorYMenorAux (x:xs) (maximo, minimo)
  | x > maximo = mayorYMenorAux xs (x, minimo)
  | x < minimo = mayorYMenorAux xs (maximo, x)
  | otherwise = mayorYMenorAux xs (maximo, minimo)


-- A3
leerHora :: String -> Either String (Int, Int)
leerHora xs
  | length xs /= 5 = Left "formato inválido" -- Verifica que el formato sea "hh:mm", es decir, que tenga 5 caracteres.
  | xs !! 2 /= ':' = Left "formato inválido" -- Verifica que el tercer carácter sea ':', que es el separador entre horas y minutos.
  | not (isDigit (xs !! 0) && isDigit (xs !! 1) && isDigit (xs !! 3) && isDigit (xs !! 4)) = Left "formato inválido" -- Verifica que los caracteres de horas y minutos sean dígitos.
  | xs !! 0 > '2' = Left "hora fuera de rango" -- Caso de 30 o más horas, que no es válido.
  | xs !! 0 == '2' && xs !! 1 > '3' = Left "hora fuera de rango" -- Caso de 24 a 29 horas, que no es válido.
  | xs !! 3 > '5' = Left "minutos fuera de rango" -- Caso de 60 o más minutos, que no es válido.
  | xs !! 0 > '2' && xs !! 1 > '3' && xs !! 3 > '5' = Right (digitToInt (xs !! 0) * 10 + digitToInt (xs !! 1), digitToInt (xs !! 3) * 10 + digitToInt (xs !! 4)) -- Reportar la hora si las horas y los minutos se salen de rango
  | otherwise = Right (digitToInt (xs !! 0) * 10 + digitToInt (xs !! 1), digitToInt (xs !! 3) * 10 + digitToInt (xs !! 4))

duracion :: String -> String -> Either String Int
duracion hora1 hora2 = 
  case (leerHora hora1, leerHora hora2) of
    (Left err, _) -> Left err -- Si hay errores, reportamos la primera hora que tenga un error.
    (_, Left err) -> Left err
    (Right (h1, m1), Right (h2, m2)) -> 
      if h1 > h2 || (h1 == h2 && m1 > m2) then Right ((h2 + 24) * 60 + m2 - (h1 * 60 + m1)) -- Si la primera hora es mayor que la segunda, asumimos que la segunda hora es del día siguiente.
      else Right (h2 * 60 + m2 - (h1 * 60 + m1)) -- Si la primera hora es menor o igual que la segunda, calculamos la diferencia en minutos.

-- =====================================================================
-- Parte B: orden superior.
-- Prohibido: recursión explícita (ninguna función de esta parte, ni sus
-- auxiliares, puede llamarse a sí misma).
-- =====================================================================

-- B1
promedioSobreUmbral :: Double -> [Double] -> Maybe Double
promedioSobreUmbral n xs = 
  let (suma, contador) = filtrarPromedio n xs
  in if contador == 0 then Nothing else Just (suma / fromIntegral contador)

filtrarPromedio :: Double -> [Double] -> (Double, Int)
filtrarPromedio n xs = foldr filtrar (0.0, 0) xs
  where
    filtrar x (suma, contador)
      | x > n = (suma + x, contador + 1)
      | otherwise = (suma, contador)

-- B2
movimientosCaballo :: (Int, Int) -> [(Int, Int)]
movimientosCaballo (fila, columna) = 
  [ (fila + df, columna + dc) -- Genera todas las combinaciones de movimientos posibles del caballo
  | df <- [-2, -1, 1, 2], dc <- [-2, -1, 1, 2], abs df /= abs dc, -- Aseguramos movimiento de "L", nos movemos 2 en una dirección y 1 en la otra
    fila + df >= 1, fila + df <= 8, columna + dc >= 1, columna + dc <= 8 ] -- Aseguramos que el movimiento esté dentro del tablero de ajedrez (1 a 8 para filas y columnas)

enDosSaltos :: (Int, Int) -> Int
enDosSaltos (fila, columna) = length (foldr movimientosUnicos [] (movimientosCaballo (fila, columna)))
  where
    movimientosUnicos (filaActual, columnaActual) acumulador =
      let nuevosMovimientos = movimientosCaballo (filaActual, columnaActual)
          movimientosFiltrados = filter (`notElem` acumulador) nuevosMovimientos
      in acumulador ++ movimientosFiltrados

-- B3
miMap :: (a -> b) -> [a] -> [b]
miMap _ [] = []
miMap f (x:xs) = foldr (\y acc -> f y : acc) [] (x:xs)

miFilter :: (a -> Bool) -> [a] -> [a]
miFilter _ [] = []
miFilter p (x:xs) = foldr (\y acc -> if p y then y : acc else acc) [] (x:xs)

miFoldl :: (b -> a -> b) -> b -> [a] -> b
miFoldl = undefined