module Main where

import Graphics.UI.Gtk
import System.Random
import qualified Game as G
import qualified DataTypes as DT
import qualified Control.Concurrent.Timer as CT
import qualified Control.Concurrent.Suspend.Lifted as SL
       
makeGrid :: IO (Table , [Button])
makeGrid = do
  table <- tableNew 7 7 True
  coordButtons <- sequence $ concat [(makeButtons 7 0 1 0 1 []), 
                                     (makeButtons 7 0 1 1 2 []), 
                                     (makeButtons 7 0 1 2 3 []), 
                                     (makeButtons 7 0 1 3 4 []), 
                                     (makeButtons 7 0 1 4 5 []), 
                                     (makeButtons 7 0 1 5 6 [])]
  lab1 <- makeLabel 0 1 6 7 "||     1     ||"
  lab2 <- makeLabel 1 2 6 7 "||     2     ||"
  lab3 <- makeLabel 2 3 6 7 "||     3     ||"
  lab4 <- makeLabel 3 4 6 7 "||     4     ||"
  lab5 <- makeLabel 4 5 6 7 "||     5     ||"
  lab6 <- makeLabel 5 6 6 7 "||     6     ||"
  lab7 <- makeLabel 6 7 6 7 "||     7     ||"
  let colLabels = [lab1,lab2,lab3,lab4,lab5,lab6,lab7]
  let buttons = fmap (\(l,r,t,b, button) -> button) coordButtons
  attachButtons <- sequence $ fmap (attachFromTuple table) coordButtons
  attachLabels <-  sequence $ fmap (attachFromTuple table) colLabels
  return (table, buttons)
  where attachFromTuple tab (l,r,t,b,button)
          = tableAttachDefaults tab button l r t b
        makeButton left right top bottom = do
          button <- buttonNew
          white <- imageNewFromFile "app/white-circle.png"
          buttonSetImage button white
          return (left,right, top, bottom, button)
        makeButtons n l r t b buttons
          | n < 1 = reverse buttons
          | otherwise = makeButtons (n-1)
                                    (l+1)
                                    (r+1)
                                    t
                                    b
                                    (makeButton l r t b:buttons)
        makeLabel left right top bottom str = do
          label <- labelNew $ Just str
          return (left, right,top,bottom,label)

clearBoard :: [Button] -> IO [Button]
clearBoard buttons = do
  clearedBtns <- sequence $ fmap clearButton buttons
  return clearedBtns
  where clearButton btn = do
          white <- imageNewFromFile "app/white-circle.png"
          buttonSetImage btn white
          return btn
          
playerTable :: IO (Table,
                   Button,
                   Label,
                   (CheckButton,CheckButton),
                   (CheckButton,CheckButton),
                   SpinButton)
playerTable = do
  table <- tableNew 2 3 True
  startButton <- buttonNewWithLabel "New Game"
  winnerTitle <- labelNew $ Just "Winner: "
  winnerName <- labelNew $ Just "??"
  -- Modality
  modalityBox <- vBoxNew True 2
  modalityTitle <- labelNew $ Just "Modality"
  modButton1 <- checkButtonNewWithLabel "Human vs Computer"
  modButton2 <- checkButtonNewWithLabel "Computer vs Computer"
  toggleButtonSetActive modButton1 True
  toggleButtonToggled modButton1
  onToggled modButton1 (switchState modButton2)
  toggleButtonToggled modButton2
  onToggled modButton2 (switchState modButton1)
  boxPackStart modalityBox modalityTitle PackNatural 0
  boxPackStart modalityBox modButton1 PackNatural 0
  boxPackStart modalityBox modButton2 PackNatural 0
  -- Color
  colorBox <- vBoxNew True 2
  colorTitle <- labelNew $ Just "Color"
  colorButton1 <- checkButtonNewWithLabel "Yellow (First)"
  colorButton2 <- checkButtonNewWithLabel "Red (Second)"
  toggleButtonSetActive colorButton1 True
  toggleButtonToggled colorButton1
  onToggled colorButton1 (switchState colorButton2)
  toggleButtonToggled colorButton2
  onToggled colorButton2 (switchState colorButton1)
  boxPackStart colorBox colorTitle PackNatural 0
  boxPackStart colorBox colorButton1 PackNatural 0
  boxPackStart colorBox colorButton2 PackNatural 0
  -- Difficulty
  diffBox <- vBoxNew True 2
  diffTitle <- labelNew $ Just "Difficulty"
  difficulty <- spinButtonNewWithRange 1 5 1
  boxPackStart diffBox diffTitle PackNatural 0
  boxPackStart diffBox difficulty PackNatural 0
  -- Attach to table
  tableAttachDefaults table startButton 0 1 0 1
  tableAttachDefaults table winnerTitle 1 2 0 1
  tableAttachDefaults table winnerName 2 3 0 1
  tableAttachDefaults table modalityBox 0 1 1 2
  tableAttachDefaults table colorBox 1 2 1 2
  tableAttachDefaults table diffBox 2 3 1 2
  -- .. --
  return (table,
          startButton,
          winnerName,
          (modButton1,modButton2),
          (colorButton1,colorButton2),
          difficulty)
  where switchState ckButt = do
          toggleButtonSetActive ckButt False

boardCoords :: [(DT.Column,DT.Row)]
boardCoords = concat [fmap (f 1 1) [0.. 6],
                      fmap (f 1 2) [0.. 6],
                      fmap (f 1 3) [0.. 6],
                      fmap (f 1 4) [0.. 6],
                      fmap (f 1 5) [0.. 6],
                      fmap (f 1 6) [0.. 6]]
  where f col row n = (col+n,row)

showWinner :: Maybe DT.Player -> String
showWinner w
  | w == Just DT.Black = "RED PLAYER!!"
  | otherwise = "YELLOW PLAYER!!"
   
findButton _ [] = Nothing
findButton (c,r) ((btn,(c',r')):btns)
  | (c,r) == (c',r') = Just btn
  | otherwise = findButton (c,r) btns

computerVsComputer :: DT.Board (DT.Slot Integer)
                      -> DT.Depth
                      -> DT.Player
                      -> [Button]
                      -> Label
                      -> IO ()
computerVsComputer board depth player buttons wName = do
  order <- randomRIO (1,7) :: IO Integer
  let rival = if player == DT.White then DT.Black else DT.White
      maybeComputerMove = G.pickBestMove board player order depth
      buttonsWithCoords = zip buttons boardCoords
  case G.gotWinner board of
    Just DT.Black -> do
      set wName [labelLabel := "RED PLAYER!!"]
      return ()
    Just DT.White -> do
      set wName [labelLabel := "YELLOW PLAYER!!"]
      return ()
    Nothing ->
      case maybeComputerMove of
        Nothing -> do
          set wName [labelLabel := "IT'S A DRAW"]
          return ()
        Just computerMove -> do
          let Just cPlayedBoard = G.makeMove board player computerMove
              Just computerButton = findButton computerMove buttonsWithCoords
          color <- if player == DT.White then imageNewFromFile "app/yellow-circle.png" else imageNewFromFile "app/red-circle.png"
          buttonSetImage computerButton color
          case G.gotWinner cPlayedBoard of
            Just DT.Black -> do
              set wName [labelLabel := "RED PLAYER!!"]
              return ()
            Just DT.White -> do
              set wName [labelLabel := "YELLOW PLAYER!!"]
              return ()
            Nothing -> do
              CT.oneShotTimer (computerVsComputer cPlayedBoard depth rival buttons wName) (SL.usDelay 500000)
              return ()

humanVsComputer :: DT.Board (DT.Slot Integer)
                   -> DT.Depth
                   -> DT.Player
                   -> [Button]
                   -> Label
                   -> IO ()
humanVsComputer board depth player buttons wName = do
  dialog <- makeDialog
  case G.gotWinner board of
    Just DT.Black -> do
      set wName [labelLabel := "RED PLAYER!!"]
      return ()
    Just DT.White -> do
      set wName [labelLabel := "YELLOW PLAYER!!"]
      return ()
    Nothing -> do
      case player of
        DT.White -> do
          order <- randomRIO (1,7) :: IO Integer
          response <- dialogRun dialog :: IO ResponseId
          widgetDestroy dialog
          let maybeColumn = getUserCol response
              moves = G.validMoves board order
              buttonsWithCoords = zip buttons boardCoords
          case maybeColumn of
            Nothing ->
              return ()
            Just column ->
              case findMove column moves of
                Nothing -> humanVsComputer board depth player buttons wName
                Just playerMove -> do
                  let Just hPlayedBoard = G.makeMove board player playerMove
                      Just playerButton = findButton playerMove buttonsWithCoords
                  yellow <- imageNewFromFile "app/yellow-circle.png"
                  buttonSetImage playerButton yellow
                  let computerMove = G.pickBestMove hPlayedBoard DT.Black order depth
                  case computerMove of
                    Nothing -> do
                      set wName [labelLabel := showWinner (G.gotWinner hPlayedBoard)]
                      return ()
                    Just computerMove -> do
                      let Just cPlayedBoard = G.makeMove hPlayedBoard DT.Black computerMove
                          Just computerButton = findButton computerMove buttonsWithCoords
                      red <- imageNewFromFile "app/red-circle.png"
                      buttonSetImage computerButton red
                      humanVsComputer cPlayedBoard depth player buttons wName
        DT.Black -> do
          order <- randomRIO (1,7) :: IO Integer
          let maybeComputerMove = G.pickBestMove board DT.White order depth
              buttonsWithCoords = zip buttons boardCoords 
          case maybeComputerMove of
            Nothing -> do
                set wName [labelLabel := showWinner (G.gotWinner board)]
                return ()
            Just computerMove -> do
              let Just cPlayedBoard = G.makeMove board DT.White computerMove
                  Just computerButton = findButton computerMove buttonsWithCoords
              yellow <- imageNewFromFile "app/yellow-circle.png"
              buttonSetImage computerButton yellow
              case G.gotWinner cPlayedBoard of
                Just DT.Black -> do
                  set wName [labelLabel := "RED PLAYER!!"]
                  return ()
                Just DT.White -> do
                  set wName [labelLabel := "YELLOW PLAYER!!"]
                  return ()
                Nothing -> do
                  let moves = G.validMoves cPlayedBoard order
                  maybePlayerMove <- forceValidMoveIfNoWinner Nothing cPlayedBoard moves
                  case maybePlayerMove of
                    Left "Black Winner" -> do
                      set wName [labelLabel := "RED PLAYER!!"]
                      return ()
                    Left "White Winner" -> do
                      set wName [labelLabel := "YELLOW PLAYER!!"]
                      return ()
                    Left "User Exit" -> return ()
                    Right playerMove -> do
                      let Just hPlayedBoard = G.makeMove cPlayedBoard player playerMove
                          Just playerButton = findButton playerMove buttonsWithCoords
                      red <- imageNewFromFile "app/red-circle.png"
                      buttonSetImage playerButton red
                      humanVsComputer hPlayedBoard depth player buttons wName
  where forceValidMoveIfNoWinner maybeCol brd moves =
          case G.gotWinner brd of
            Just DT.Black -> return $ Left "Black Winner"
            Just DT.White -> return $ Left "White Winner"
            Nothing ->
              case maybeCol of
                Nothing -> do
                  dialog <- makeDialog
                  response <- dialogRun dialog :: IO ResponseId
                  widgetDestroy dialog
                  let maybeColumn = getUserCol response
                  case maybeColumn of
                    Nothing -> return $ Left "User Exit"
                    Just column ->
                      case findMove column moves of
                        Nothing -> forceValidMoveIfNoWinner Nothing brd moves 
                        Just playerMove -> return $  Right playerMove
                Just column ->
                  case findMove column moves of
                    Nothing -> forceValidMoveIfNoWinner Nothing brd moves
                    Just playerMove -> return $ Right playerMove
        findMove col [] = Nothing 
        findMove col ((c,r):mvs)
          | col == c = Just (c,r)
          | otherwise = findMove col mvs        
        getUserCol res
          | res == ResponseUser 1 = Just 1 
          | res == ResponseUser 2 = Just 2
          | res == ResponseUser 3 = Just 3 
          | res == ResponseUser 4 = Just 4 
          | res == ResponseUser 5 = Just 5 
          | res == ResponseUser 6 = Just 6 
          | res == ResponseUser 7 = Just 7 
          | otherwise = Nothing
        makeDialog = do
          dia <- dialogNew 
          set dia [windowTitle := "Pick a column!!"]
          set dia [windowWindowPosition := WinPosMouse]
          dialogAddButton dia "Column 1" (ResponseUser 1)
          dialogAddButton dia "Column 2" (ResponseUser 2)
          dialogAddButton dia "Column 3" (ResponseUser 3)
          dialogAddButton dia "Column 4" (ResponseUser 4)
          dialogAddButton dia "Column 5" (ResponseUser 5)
          dialogAddButton dia "Column 6" (ResponseUser 6)
          dialogAddButton dia "Column 7" (ResponseUser 7)
          return dia
  
playConnect4 :: CheckButton
                -> CheckButton
                -> SpinButton
                -> DT.Board (DT.Slot Integer)
                -> [Button]
                -> Label
                -> IO ()
playConnect4 modBtn plyBtn diffBtn board buttons wName = do
  set wName [labelLabel := "??"]
  clearedButtons <- clearBoard buttons
  difficulty <- spinButtonGetValueAsInt diffBtn 
  isHvsH <- toggleButtonGetActive modBtn
  isYellow <- toggleButtonGetActive plyBtn
  let player = if isYellow then DT.White else DT.Black
  case isHvsH of
    True -> humanVsComputer board (fromIntegral difficulty) player buttons wName
    False -> computerVsComputer board (fromIntegral difficulty) DT.White buttons wName
  
main = do
  initGUI
  window <- windowNew
  mainBox <- vBoxNew False 10
  (board, boardButtons) <- makeGrid
  sep <- hSeparatorNew
  (gameTable,
   startButton,
   winnerName,
   (mod1,mod2),
   (colorY,colorR),
   diffButton) <- playerTable
  set window [windowTitle := "Connect 4!",
              windowWindowPosition := WinPosCenter,
              windowDefaultWidth := 200,
              windowDefaultHeight := 650,
              containerChild := mainBox]  
  boxPackStart mainBox board PackGrow 0
  boxPackStart mainBox sep PackNatural 0
  boxPackStart mainBox gameTable PackNatural 0 
  onDestroy window mainQuit
  mod1St <- toggleButtonGetActive mod1
  mod2St <- toggleButtonGetActive mod1
  colorYSt <- toggleButtonGetActive colorY
  colorRSt <- toggleButtonGetActive colorR
  onClicked startButton (playConnect4 mod1 colorY diffButton G.gameBoard boardButtons winnerName)
  widgetShowAll window
  mainGUI




