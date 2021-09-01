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
checaVencedor tabuleiro = primeiroJusts $ map checaLinhas todasAsLinhas
  where
    todasAsLinhas = tabuleiro ++ transpose tabuleiro ++ diagonais tabuleiro

checaEmpate :: [Maybe Player] -> Int -> Bool 
checaEmpate lista 0 = if isNothing (lista!!0) then False else True
checaEmpate lista i = if isNothing (lista!!i) then False else checaEmpate lista (i-1)

vetorizaTabuleiro :: Tabuleiro -> Int -> [Maybe Player] -> [Maybe Player]
vetorizaTabuleiro tabuleiro 0 lista = lista ++ (tabuleiro!!0)
vetorizaTabuleiro tabuleiro i lista = vetorizaTabuleiro tabuleiro (i-1) (lista ++ (tabuleiro!!i))

checaLinhas :: [Maybe Player] -> Maybe Player
checaLinhas linha
    | null vencedores = Nothing
    | otherwise = head $ head vencedores
  where
    vencedores = filter ((>= larguraPadraoVencedor) . length) (group linha)

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
