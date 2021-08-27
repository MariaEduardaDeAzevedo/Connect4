module Game where
import DataTypes 
import Data.Monoid


-- This function converts a board into a list.
boardToList b = [slot11 b,slot21 b,slot31 b,slot41 b,slot51 b,slot61 b,slot71 b,
                 slot12 b,slot22 b,slot32 b,slot42 b,slot52 b,slot62 b,slot72 b,
                 slot13 b,slot23 b,slot33 b,slot43 b,slot53 b,slot63 b,slot73 b,
                 slot14 b,slot24 b,slot34 b,slot44 b,slot54 b,slot64 b,slot74 b,
                 slot15 b,slot25 b,slot35 b,slot45 b,slot55 b,slot65 b,slot75 b,
                 slot16 b,slot26 b,slot36 b,slot46 b,slot56 b,slot66 b,slot76 b]


-- This function returns the Board slot for the given position.
getSlot :: Board (Slot Integer) -> (Column,Row) -> Maybe (Slot Integer)
getSlot board (col,row)
  | (col < 1) || (col > 7) = Nothing -- Columns out of board positions.
  | (row < 1) || (row > 6) = Nothing -- Columns out of board positions.                           
  | otherwise = Just $ findSlot 1 (boardToList board)
  where slotPos = (7 * row) - (7 - col)
        findSlot pos (sl:sls)
          | pos == slotPos = sl
          | otherwise = findSlot (pos + 1) sls
        
gotWinner :: Board (Slot Integer) -> Maybe Player
gotWinner board
  | not (null rows) = head rows
  | not (null cols) = head cols
  | not (null x6Diagonals) = head x6Diagonals    
  | not (null x5Diagonals) = head x5Diagonals
  | not (null x4Diagonals) = head x4Diagonals
  | otherwise = Nothing  -- No winner
  where isPlayer slot = case slot of
          Played p -> Just p
          _ -> Nothing
        allEqual s slots = all (\v -> v == s) slots
        x7Winner (s1,s2,s3,s4,s5,s6,s7)
          | allEqual s1 [s1,s2,s3,s4] = isPlayer s1
          | allEqual s2 [s2,s3,s4,s5] = isPlayer s2
          | allEqual s3 [s3,s4,s5,s6] = isPlayer s3
          | allEqual s4 [s4,s5,s6,s7] = isPlayer s4
          | otherwise = Nothing
        x6Winner (s1,s2,s3,s4,s5,s6)
          | allEqual s1 [s1,s2,s3,s4] = isPlayer s1
          | allEqual s2 [s2,s3,s4,s5] = isPlayer s2
          | allEqual s3 [s3,s4,s5,s6] = isPlayer s3
          | otherwise = Nothing
        x5Winner (s1,s2,s3,s4,s5) 
          | allEqual s1 [s1,s2,s3,s4] = isPlayer s1
          | allEqual s2 [s2,s3,s4,s5] = isPlayer s2
          | otherwise = Nothing
        x4Winner (s1,s2,s3,s4)
          | allEqual s1 [s1,s2,s3,s4] = isPlayer s1
          | otherwise = Nothing
        (Board s11 s21 s31 s41 s51 s61 s71
               s12 s22 s32 s42 s52 s62 s72
               s13 s23 s33 s43 s53 s63 s73
               s14 s24 s34 s44 s54 s64 s74
               s15 s25 s35 s45 s55 s65 s75
               s16 s26 s36 s46 s56 s66 s76) = board
        notNothing mV = mV /= Nothing                                      
        rows = filter notNothing $ fmap x7Winner [(s11,s21,s31,s41,s51,s61,s71), (s12,s22,s32,s42,s52,s62,s72),
                                                  (s13,s23,s33,s43,s53,s63,s73), (s14,s24,s34,s44,s54,s64,s74),
                                                  (s15,s25,s35,s45,s55,s65,s75), (s16,s26,s36,s46,s56,s66,s76)]
        cols = filter notNothing $ fmap x6Winner [(s11,s12,s13,s14,s15,s16), (s21,s22,s23,s24,s25,s26), 
                                                  (s31,s32,s33,s34,s35,s36), (s41,s42,s43,s44,s45,s46), 
                                                  (s51,s52,s53,s54,s55,s56),  (s61,s62,s63,s64,s65,s66), 
                                                  (s71,s72,s73,s74,s75,s76)] 
        x6Diagonals = filter notNothing $ fmap x6Winner [(s16,s25,s34,s43,s52,s61), (s26,s35,s44,s53,s62,s71),   
                                                         (s11,s22,s33,s44,s55,s66), (s21,s32,s43,s54,s65,s76)] 
        x5Diagonals = filter notNothing $ fmap x5Winner [(s12,s23,s34,s45,s56), (s31,s42,s53,s64,s75),   
                                                         (s15,s24,s33,s42,s51), (s36,s45,s54,s63,s72)] 
        x4Diagonals = filter notNothing $ fmap x4Winner [(s14,s23,s32,s41), (s46,s55,s64,s73),
                                                         (s13,s24,s35,s46), (s41,s52,s63,s74)]
                         
-- This function returns true if the given move is valid for the given board
isValid :: Board (Slot Integer) -> (Column,Row) -> Bool
isValid board (col,row)
  | slot == Nothing = False
  | (slot == Just (Played White)) || (slot == Just (Played Black)) = False -- That position has already been played.
  | any id (fmap isEmpty belowSlots) = False -- All slots below (col,row) must already have been played.
  | gotWinner board /= Nothing = False -- This board already has a winner.
  | slot /= Just (Position col row) = False -- An slot that does not match its position.                               
  | otherwise = True
  where slot = getSlot board (col,row)
        genBelowCoords coords (c,r)
          | r == 6 = coords
          | otherwise = genBelowCoords ((c,r+1):coords) (c,r+1)
        belowSlots = fmap (getSlot board) (genBelowCoords [] (col,row)) 
        isEmpty maybeSlot =
          case maybeSlot of
            Just (Position a b) -> True
            _ -> False


-- Given a board, a player and a move position, this function
-- returns a new Board with the new move applied
makeMove :: Board (Slot Integer) -> Player -> (Column,Row) -> Maybe (Board (Slot Integer))
makeMove board player (col,row) =
  case isValid board (col,row) of
    False  -> Nothing
    True -> Just $ fmap match board
  where match slot
          | slot == (Position col row) = Played player
          | otherwise = slot

                        
-- This function returns a list of valid moves for a given Board.
validMoves :: Board (Slot Integer) -> MoveOrder -> [(Column,Row)]
validMoves board n 
  | n == 1 =  filter (isValid board) moves1
  | n == 2 = filter (isValid board) moves2
  | n == 3 = filter (isValid board) moves3
  | n == 4 = filter (isValid board) moves4
  | n == 5 = filter (isValid board) moves5
  | n == 6 = filter (isValid board) moves6
  | n == 7 = filter (isValid board) moves7
  | otherwise = filter (isValid board) moves4
  where moves1 = [(c,r) | c <- [1,2,3,4,5,6,7], r <- [5,4,3,2,1,6]]
        moves2 = [(c,r) | c <- [2,3,1,4,7,6,5], r <- [1,2,3,4,5,6]]
        moves3 = [(c,r) | c <- [3,2,1,7,5,4,6], r <- [1,6,3,4,5,2]]
        moves4 = [(c,r) | c <- [4,2,3,1,5,6,7], r <- [2,1,4,3,5,6]]
        moves5 = [(c,r) | c <- [5,1,3,4,7,6,2], r <- [1,2,3,5,4,6]]
        moves6 = [(c,r) | c <- [6,2,4,3,5,1,7], r <- [6,2,3,4,5,1]]
        moves7 = [(c,r) | c <- [7,2,5,4,3,6,1], r <- [1,2,6,4,5,3]]
       
       
-- Given a Board, a Player and a depth, this function returns the game tree for this player
-- with the specified depth.
genGameTree :: Board (Slot Integer)
               -> Player
               -> MoveOrder
               -> Depth
               -> GameTree (Board (Slot Integer))
genGameTree board player order depth = buildTree (Just board) player depth
  where maybe7Moves mvs
          | size > 7 = take 7 maybeMoves
          | otherwise = insertNothing (7 - size) maybeMoves
          where size = length mvs
                maybeMoves = fmap Just mvs
                insertNothing 0 ls = ls
                insertNothing i ls = insertNothing (i - 1) (Nothing : ls)       
        buildTree maybeBoard player depth =
          case maybeBoard of
            Nothing -> Leaf
            Just board ->
              let [m1,m2,m3,m4,m5,m6,m7] = maybe7Moves (validMoves board order)
              in case depth < 1 of
                   True -> Node board Leaf Leaf Leaf Leaf Leaf Leaf Leaf
                   False -> Node board (buildTree (maybeMakeMove board player m1) rival depth')
                                       (buildTree (maybeMakeMove board player m2) rival depth')
                                       (buildTree (maybeMakeMove board player m3) rival depth')
                                       (buildTree (maybeMakeMove board player m4) rival depth')
                                       (buildTree (maybeMakeMove board player m5) rival depth')
                                       (buildTree (maybeMakeMove board player m6) rival depth')
                                       (buildTree (maybeMakeMove board player m7) rival depth')
          where rival = if player == White then Black else White
                depth' = depth - 1
                maybeMakeMove brd plyr mbMv = do
                  mv <- mbMv
                  makeMove brd plyr mv


-- This function returns 1 if the current player is a winner, -1 if its rival is a winner
-- and 0 otherwise.

scoreBoard :: Player -> Board (Slot Integer) -> Integer
scoreBoard player board
  | winner == (Just player) = 1
  | winner == (Just rival) = -1                           
  | otherwise = 0
  where winner = gotWinner board
        rival = if player == White then Black else White



isTerminalBranch :: GameTree (Board (Slot Integer)) -> Bool
isTerminalBranch branch = all (\v -> v == Leaf) [b1,b2,b3,b4,b5,b6,b7]
  where (Node v b1 b2 b3 b4 b5 b6 b7) = branch


pickBoundIfFound :: (GameTree (Board (Slot Integer)) -> Score)
                    -> Bound
                    -> GameTree (Board (Slot Integer))
                    -> GameTree (Board (Slot Integer))
                    -> GameTree (Board (Slot Integer))
                    -> GameTree (Board (Slot Integer))
                    -> GameTree (Board (Slot Integer))
                    -> GameTree (Board (Slot Integer))
                    -> GameTree (Board (Slot Integer))
                    -> Score
pickBoundIfFound f bound b1 b2 b3 b4 b5 b6 b7
  | scores !! 0 == bound = bound
  | scores !! 1 == bound = bound
  | scores !! 2 == bound = bound
  | scores !! 3 == bound = bound
  | scores !! 4 == bound = bound
  | scores !! 5 == bound = bound
  | scores !! 6 == bound = bound
  | scores !! 0 == 0 = 0
  | scores !! 1 == 0 = 0
  | scores !! 2 == 0 = 0
  | scores !! 3 == 0 = 0
  | scores !! 4 == 0 = 0
  | scores !! 5 == 0 = 0
  | scores !! 6 == 0 = 0
  | otherwise = negate bound
  where scores = [f b1,f b2,f b3,f b4,f b5,f b6,f b7] 


findNodeMaxScore :: Player -> GameTree (Board (Slot Integer)) -> Score                                     
findNodeMaxScore player gameTree =
  case gameTree of
    Leaf -> 0
    _ -> let (Node board b1 b2 b3 b4 b5 b6 b7) = gameTree
         in case isTerminalBranch gameTree of
              True -> scoreBoard player board
              False -> pickBoundIfFound (findNodeMinScore player) 1 b1 b2 b3 b4 b5 b6 b7
                       
findNodeMinScore :: Player -> GameTree (Board (Slot Integer)) -> Score
findNodeMinScore player gameTree =
  case gameTree of
    Leaf -> 0
    _ -> let (Node board b1 b2 b3 b4 b5 b6 b7) = gameTree
         in case isTerminalBranch gameTree of
               True -> scoreBoard player board
               False -> pickBoundIfFound (findNodeMaxScore player) (-1) b1 b2 b3 b4 b5 b6 b7
                                 
            
evaluateBoardMax :: Player -> MoveOrder -> Depth -> Board (Slot Integer) -> Integer
evaluateBoardMax player order depth board  = findNodeMinScore player gameTree
  where gameTree = genGameTree board rival order depth 
        rival = if player == White then Black else White
        
scoreMoves :: Board (Slot Integer) -> Player -> MoveOrder -> Depth -> Maybe [((Column,Row), Integer)]
scoreMoves board player order depth = do
  boards <- sequence $ fmap (makeMove board player) vMoves
  case boards of
    [] -> Nothing
    _ -> let scores = fmap (evaluateBoardMax player order depth) boards
         in return $ zip vMoves scores
  where vMoves = validMoves board order   

pickBestMove :: Board (Slot Integer) -> Player -> MoveOrder -> Depth -> Maybe (Column,Row)
pickBestMove board player order depth = do
  (score:scores) <- scoreMoves board player order depth
  let (pos, scr) = foldr maxScore score scores
  return pos
  where maxScore (p1,s1) (p2,s2)
          | s1 > s2 = (p1,s1)
          | otherwise = (p2,s2)

gameBoard :: Board (Slot Integer)
gameBoard = Board (Position 1 1) (Position 2 1) (Position 3 1) (Position 4 1) (Position 5 1) (Position 6 1) (Position 7 1) 
                  (Position 1 2) (Position 2 2) (Position 3 2) (Position 4 2) (Position 5 2) (Position 6 2) (Position 7 2)
                  (Position 1 3) (Position 2 3) (Position 3 3) (Position 4 3) (Position 5 3) (Position 6 3) (Position 7 3)
                  (Position 1 4) (Position 2 4) (Position 3 4) (Position 4 4) (Position 5 4) (Position 6 4) (Position 7 4)
                  (Position 1 5) (Position 2 5) (Position 3 5) (Position 4 5) (Position 5 5) (Position 6 5) (Position 7 5)
                  (Position 1 6) (Position 2 6) (Position 3 6) (Position 4 6) (Position 5 6) (Position 6 6) (Position 7 6)

