module Utils.Diagonais where
import Utils.Colunas

import Data.Maybe (isJust)

primeiroJusts :: [Maybe a] -> Maybe a
primeiroJusts xs
    | null justs = Nothing
    | otherwise = head justs
  where
    justs = filter isJust xs

diagonalPrimaria :: Int -> Int -> [(Int, Int)]
diagonalPrimaria width altura = zip [0 .. altura - 1] [0 .. width - 1]

getDiagonalPrimaria :: Int -> Int -> [[(Int, Int)]]
getDiagonalPrimaria altura width =
    init $
    filter (not . null) $
    map (filter (\(linha, coluna) -> linha < altura && coluna < width)) $
    metadeInferior ++ map (map (\(f, s) -> (s, f))) metadeInferior
  where
    ordem = max altura width
    metadeInferior =
      map (\x -> map (\(f, s) -> (f + ordem - x, s)) $ diagonalPrimaria x x)
        [1 .. ordem]

diagonalSecundaria :: Int -> Int -> [(Int, Int)]
diagonalSecundaria width altura =
  zip [altura - 1,altura - 2 .. 0] [0 .. width - 1]

getDiagonalSecundaria :: Int -> Int -> [[(Int, Int)]]
getDiagonalSecundaria altura width =
    init $
    filter (not . null) $
    map (filter (\(linha, coluna) -> linha < altura && coluna < width)) $
    metadeSuperior ++ map (map (\(f, s) -> (ordem - s - 1, ordem - f - 1))) metadeSuperior
  where
    ordem = max altura width
    metadeSuperior = map (\x -> diagonalSecundaria x x) [1 .. ordem]

diagonalEsqDir :: [[a]] -> [[a]]
diagonalEsqDir matriz =
  map (map (`getDimensao` matriz)) $ getDiagonalPrimaria (length matriz) (length $ head matriz)

diagonalDirEsq :: [[a]] -> [[a]]
diagonalDirEsq matriz =
  map (map (`getDimensao` matriz)) $
  getDiagonalSecundaria (length matriz) (length $ head matriz)

diagonais :: [[a]] -> [[a]]
diagonais matriz = diagonalEsqDir matriz ++ diagonalDirEsq matriz

cycleData :: Enum a => a -> a
cycleData cur = allValues !! ((fromEnum cur + 1) `mod` length allValues)
  where
    allValues = [toEnum 0 ..]
