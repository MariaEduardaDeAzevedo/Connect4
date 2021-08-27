module Jogo.Connect4 where

import Data.List  (group)
import Data.Maybe (catMaybes, fromJust, isJust, isNothing)
import Utils.Colunas
import Utils.Diagonais

data Player
  = Red
  | Yellow
  deriving (Eq, Enum)

instance Show Player where
  show Red = "R"
  show Yellow = "Y"

type Tabuleiro = [[Maybe Player]]

type Column = Int

data Game = Game
  { jogoTabuleiro  :: Tabuleiro
  , getJogadorVez :: Player
  , getVencedor :: Maybe Player
  } deriving (Show)

larguraPadrao :: Int
larguraPadrao = 6

alturaPadrao :: Int
alturaPadrao = 7

larguraPadraoVencedor :: Int
larguraPadraoVencedor = 4

tabuleiroVazio :: Tabuleiro
tabuleiroVazio = replicate alturaPadrao (replicate larguraPadrao Nothing)

jogoInicial :: Game
jogoInicial = Game tabuleiroVazio Red Nothing

caiPeca :: Tabuleiro -> Column -> Int
caiPeca tabuleiro coluna = length (takeWhile isNothing (getColuna coluna tabuleiro)) - 1

checaVencedor :: Tabuleiro -> Maybe Player
checaVencedor tabuleiro = primeiroJusts $ map checaLinhas allLines
  where
    allLines = tabuleiro ++ transpose tabuleiro ++ diagonais tabuleiro

checaLinhas :: [Maybe Player] -> Maybe Player
checaLinhas linha
    | null winners = Nothing
    | otherwise = head $ head winners
  where
    winners = filter ((>= larguraPadraoVencedor) . length) (group linha)

mover :: Player -> Column -> Tabuleiro -> Tabuleiro
mover jogador coluna tabuleiro
  | caiPeca tabuleiro coluna >= 0 = trocaPosicaoDimensao (caiPeca tabuleiro coluna) coluna (Just jogador) tabuleiro
  | otherwise = tabuleiro

jogar :: Column -> Game -> Game
jogar coluna jogo
    | isJust (getVencedor jogo) = jogo
    | novoTabuleiro == jogoTabuleiro jogo = jogo
    | otherwise = Game novoTabuleiro novoJogador (checaVencedor novoTabuleiro)
  where
    novoTabuleiro = mover (getJogadorVez jogo) coluna (jogoTabuleiro jogo)
    vencedor = checaVencedor novoTabuleiro
    novoJogador
      | isNothing vencedor = cycleData (getJogadorVez jogo)
      | otherwise = getJogadorVez jogo

zipWithFiltro :: (a -> b -> Maybe c) -> [a] -> [b] -> [c]
zipWithFiltro filtro as bs = catMaybes $ zipWith filtro as bs

colunasDisponiveis :: Tabuleiro -> [Column]
colunasDisponiveis tabuleiro =
  zipWithFiltro
    (\coluna idx ->
      if any isNothing coluna
        then Just idx
        else Nothing)
    (transpose tabuleiro)
    [0 ..]
