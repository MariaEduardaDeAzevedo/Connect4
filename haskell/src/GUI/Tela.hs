module GUI.Tela where
import Data.Maybe
import Jogo.Connect4
import Graphics.Gloss

larguraDaTela :: Float
larguraDaTela = 1000

alturaDaTela :: Float
alturaDaTela = 600

larguraDoTabuleiro :: Float
larguraDoTabuleiro = 600

alturaDoTabuleiro:: Float
alturaDoTabuleiro = 600

larguraDaCelula :: Float
larguraDaCelula = larguraDoTabuleiro / fromIntegral larguraPadrao

alturaDaCelula :: Float
alturaDaCelula = alturaDoTabuleiro / fromIntegral alturaPadrao

foreground :: Color
foreground = makeColorI 0 178 232 255

corDosJogadores :: Player -> Color
corDosJogadores Red    = makeColorI 207 28 0 255
corDosJogadores Yellow = makeColorI 237 217 0 255

corEspacoVazio :: Color
corEspacoVazio = makeColorI 255 255 255 255

background :: Color
background = makeColorI 0 178 232 255