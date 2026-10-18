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
esPalindromo = texto = limpio == invertir limpio
  where 
   limpio = limpiar texto

limpiar :: String -> String
limpiar []=[] 
limpiar (x:xs)
  isAlphaNum x = toLower x: limpiar xs
  otherwise = limpiar xs

invertir :: String -> String
invertir []=[]
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
