-- =====================================================================
--  CI-3661 — Tarea 1 (Haskell) — Septiembre–Diciembre 2026
--
--  Nombre: Nazareth Colmenares
--  Carnet: 20-10017
--
--  Declaración de uso de herramientas de IA (obligatoria, ver enunciado):
-- 
--
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
esPalindromo texto = limpio == invertir limpio
  where
    limpio = limpiar texto

limpiar :: String -> String
limpiar [] = []
limpiar (x:xs)
  | isAlphaNum x = toLower x : limpiar xs
  | otherwise = limpiar xs

invertir :: String -> String
invertir [] = []
invertir (x:xs) = invertir xs ++ [x]

-- A2
-- Uso el primer elemento como mayor y menor iniciales.
-- El auxiliar procesa una vez cada elemento restante y actualiza
-- ambos valores sin volver a recorrer la lista.

mayorYMenor :: Ord a => [a] -> Maybe (a, a)
mayorYMenor [] = Nothing
mayorYMenor (x:xs) = buscarExtremos xs x x

buscarExtremos :: Ord a => [a] -> a -> a -> Maybe (a, a)
buscarExtremos [] mayor menor = Just (mayor, menor)
buscarExtremos (x:xs) mayor menor
  | x > mayor = buscarExtremos xs x menor
  | x < menor = buscarExtremos xs mayor x
  | otherwise = buscarExtremos xs mayor menor

-- A3
leerHora :: String -> Either String (Int, Int)
leerHora [a, b, ':', c, d]
  | not (isDigit a && isDigit b && isDigit c && isDigit d) =
      Left "formato inválido"
  | hora > 23 = Left "hora fuera de rango"
  | minutos > 59 = Left "minutos fuera de rango"
  | otherwise = Right (hora, minutos)
  where
    hora = digitToInt a * 10 + digitToInt b
    minutos = digitToInt c * 10 + digitToInt d
leerHora _ = Left "formato inválido"

duracion :: String -> String -> Either String Int
duracion inicio fin =
  case leerHora inicio of
    Left mensaje -> Left mensaje
    Right (h1, m1) ->
      case leerHora fin of
        Left mensaje -> Left mensaje
        Right (h2, m2) ->
          let minutosInicio = h1 * 60 + m1
              minutosFin = h2 * 60 + m2
              diferencia = minutosFin - minutosInicio
          in if diferencia < 0
             then Right (diferencia + 1440)
             else Right diferencia


-- =====================================================================
-- Parte B: orden superior.
-- Prohibido: recursión explícita (ninguna función de esta parte, ni sus
-- auxiliares, puede llamarse a sí misma).
-- =====================================================================

-- B1
promedioSobreUmbral :: Double -> [Double] -> Maybe Double
promedioSobreUmbral umbral numeros =
  case foldl (acumular umbral) (0, 0) numeros of
    (_, 0) -> Nothing
    (suma, cantidad) -> Just (suma / cantidad)

acumular :: Double -> (Double, Double) -> Double -> (Double, Double)
acumular umbral (suma, cantidad) x
  | x > umbral = (suma + x, cantidad + 1)
  | otherwise = (suma, cantidad)

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
