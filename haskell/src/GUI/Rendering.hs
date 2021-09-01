module GUI.Rendering where

import Data.Maybe
import Jogo.Connect4
import Graphics.Gloss
import GUI.Tela
    
renderizarJogo :: Game -> Picture
renderizarJogo jogo =
      pictures  
        [translate (-larguraDaTela / 2) (-alturaDaTela / 2) $
        pictures $ divisores jogo : renderizarTabuleiro (jogoTabuleiro jogo), renderizarMenu jogo]

renderizarMenu :: Game -> Picture
renderizarMenu jogo = pictures [translate (150) (200) $ scale 0.5 0.5 $ text "Connect4",
                          translate (140) (150) $ scale 0.2 0.2 $ text "Pressione 'R' para reiniciar",
                          translate (140) (110) $ scale 0.2 0.2 $ text "Pressione 'ESC' para fechar",
                          translate (150) (-10) $ color (corDosJogadores (getJogadorVez jogo)) $ scale 0.3 0.3 $ text (getTextoRodada jogo),
                          translate (380) (0) $ color (corDosJogadores (getJogadorVez jogo)) $ circleSolid 40]
                          
getTextoRodada :: Game -> String 
getTextoRodada jogo 
  | checaEmpate (vetorizaTabuleiro(jogoTabuleiro jogo) 6 []) 41 = "EMPATOU!"
  | isNothing(getVencedor jogo) = "Vez do"
  | otherwise = "VENCEU! "

renderizarCelula :: Maybe Player -> (Int, Int) -> Picture
renderizarCelula Nothing  = color (corEspacoVazio) . renderizarCirculo
renderizarCelula (Just p) = color (corDosJogadores p) . renderizarCirculo

renderizarCirculo :: (Int, Int) -> Picture
renderizarCirculo (line, col) =
        translate
                (fromIntegral col * larguraDaCelula + (larguraDaCelula / 2))
                (fromIntegral (alturaPadrao - line - 1) * alturaDaCelula +
                 (alturaDaCelula / 2)) $
        circleSolid diameter
  where
    diameter = diameterParam - 0.1 * diameterParam
    diameterParam = min (larguraDaCelula / 2) (alturaDaCelula / 2)

renderizarTabuleiro :: Tabuleiro -> [Picture]
renderizarTabuleiro b =
        concat $
        zipWith (\line lineNum ->
                         zipWith
                                 (\player colNum ->
                                          renderizarCelula player (lineNum, colNum))
                                 line
                                 [0 .. length (b !! lineNum)])
                b
                [0 .. length b]

divisores :: Game -> Picture
divisores Game {getVencedor = w} =
  color (makeColorI 0 0 0 0) $
  pictures [divisoresHorizontais, divisoresVerticais]

divisoresVerticais :: Picture
divisoresVerticais =
  pictures $
  map (\x -> line [(0, x * alturaDaCelula), (larguraDoTabuleiro, x * alturaDaCelula)])
    [0,1 .. fromIntegral alturaPadrao]

divisoresHorizontais :: Picture
divisoresHorizontais =
  pictures $
  map (\x -> line [(x * larguraDaCelula, 0), (x * larguraDaCelula, alturaDoTabuleiro)])
    [0,1 .. fromIntegral larguraPadrao]
