module GUI.ConnectGUI where
import Jogo.Connect4
import qualified Jogo.Connect4
import Graphics.Gloss
import qualified Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import GUI.Rendering
import GUI.Tela
import System.Exit (exitSuccess, exitWith)
import qualified System.Exit as System

rodarGUI :: IO ()
rodarGUI =
  play tela
    background
    60
    jogoInicial
    renderizarJogo
    eventHandler
    (const id)

tela :: Display
tela = InWindow "Connect4" (round larguraDaTela, round alturaDaTela) (200, 200)

eventHandler :: Event -> Game -> Game
eventHandler (EventKey (MouseButton LeftButton) Up _ (x, _)) jogo
    | celulaClicada `elem` colunasDisponiveis (jogoTabuleiro jogo) = jogar celulaClicada jogo
    | otherwise = jogo
  where
    celulaClicada = ceiling $ ((x + larguraDaTela / 2) / larguraDaCelula) - 1
eventHandler (EventKey (Char 'r') Up _ _) _ = jogoInicial
eventHandler (EventKey (Char 'R') Up _ _) _ = jogoInicial
eventHandler _ jogo = jogo
