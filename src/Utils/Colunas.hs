module Utils.Colunas where

getColuna :: Int -> [[a]] -> [a]
getColuna c = map (!! c)

transpose :: [[a]] -> [[a]]
transpose matriz = map (`getColuna` matriz) [0 .. length (head matriz) - 1]

trocaPosicao :: Int -> a -> [a] -> [a]
trocaPosicao n new list
    | n > length list = error "Connect4.trocaPosicao: Index out of range"
    | otherwise = start ++ (new : tail end)
  where
    (start, end) = splitAt n list

trocaPosicaoDimensao :: Int -> Int -> a -> [[a]] -> [[a]]
trocaPosicaoDimensao linha coluna new matriz = trocaPosicao linha (trocaPosicao coluna new (matriz !! linha)) matriz

getDimensao :: (Int, Int) -> [[a]] -> a
getDimensao (linha, coluna) matriz = (matriz !! linha) !! coluna
