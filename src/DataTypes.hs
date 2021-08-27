module DataTypes where

import Data.Monoid

type Column = Integer
type Row = Integer
type Depth = Integer
type MoveOrder = Integer
type Bound = Integer
type Score = Integer

data Player = Black | White deriving (Eq, Show)

data Slot a = Position a a | Played Player deriving Eq


instance Show a => Show (Slot a) where
  show slot =
    case slot of
      Position a b -> show (a,b) 
      (Played player) -> show player


data Board a =
  Board { slot11::a,slot21::a,slot31::a,slot41::a,slot51::a,slot61::a,slot71::a,
          slot12::a,slot22::a,slot32::a,slot42::a,slot52::a,slot62::a,slot72::a,
          slot13::a,slot23::a,slot33::a,slot43::a,slot53::a,slot63::a,slot73::a,
          slot14::a,slot24::a,slot34::a,slot44::a,slot54::a,slot64::a,slot74::a,
          slot15::a,slot25::a,slot35::a,slot45::a,slot55::a,slot65::a,slot75::a,
          slot16::a,slot26::a,slot36::a,slot46::a,slot56::a,slot66::a,slot76::a }
     deriving Eq


instance Functor Board where
  fmap f board = Board (f s11) (f s21) (f s31) (f s41) (f s51) (f s61) (f s71)
                       (f s12) (f s22) (f s32) (f s42) (f s52) (f s62) (f s72)
                       (f s13) (f s23) (f s33) (f s43) (f s53) (f s63) (f s73)
                       (f s14) (f s24) (f s34) (f s44) (f s54) (f s64) (f s74)
                       (f s15) (f s25) (f s35) (f s45) (f s55) (f s65) (f s75)
                       (f s16) (f s26) (f s36) (f s46) (f s56) (f s66) (f s76)
    where (Board s11 s21 s31 s41 s51 s61 s71
                 s12 s22 s32 s42 s52 s62 s72
                 s13 s23 s33 s43 s53 s63 s73
                 s14 s24 s34 s44 s54 s64 s74
                 s15 s25 s35 s45 s55 s65 s75
                 s16 s26 s36 s46 s56 s66 s76) = board


instance Show a => Show (Board a) where
    show (Board s11 s21 s31 s41 s51 s61 s71
                s12 s22 s32 s42 s52 s62 s72
                s13 s23 s33 s43 s53 s63 s73
                s14 s24 s34 s44 s54 s64 s74
                s15 s25 s35 s45 s55 s65 s75
                s16 s26 s36 s46 s56 s66 s76) =
      "                         " ++ "\n" ++
      "                " ++  "+++++++++++++++++++++++++++++++++++++++++++++++++++++" ++ "\n" ++
      showRow s11 s21 s31 s41 s51 s61 s71 ++
      showRow s12 s22 s32 s42 s52 s62 s72 ++
      showRow s13 s23 s33 s43 s53 s63 s73 ++
      showRow s14 s24 s34 s44 s54 s64 s74 ++ 
      showRow s15 s25 s35 s45 s55 s65 s75 ++
      showRow s16 s26 s36 s46 s56 s66 s76 ++
      "                                                                            " ++ "\n" 
      where showRow s1 s2 s3 s4 s5 s6 s7 =
              "                " ++ (show s1) ++ " | " ++
                                    (show s2) ++ " | " ++
                                    (show s3) ++ " | " ++
                                    (show s4) ++ " | " ++
                                    (show s5) ++ " | " ++
                                    (show s6) ++ " | " ++
                                    (show s7) ++ "\n" ++
              "                " ++  "+++++++++++++++++++++++++++++++++++++++++++++++++++++" ++ "\n" 



-- A 7-ary Tree for our connect-4 game.
data GameTree a =
      Leaf
    | Node a (GameTree a) (GameTree a) (GameTree a) (GameTree a) (GameTree a) (GameTree a) (GameTree a) 
    deriving Eq

instance Functor GameTree where
  fmap _ Leaf = Leaf
  fmap f (Node v b1 b2 b3 b4 b5 b6 b7)
    = Node (f v)
           (fmap f b1)
           (fmap f b2)
           (fmap f b3)
           (fmap f b4)
           (fmap f b5)
           (fmap f b6)
           (fmap f b7) 

instance Foldable GameTree where
  foldMap _ Leaf = mempty
  foldMap f (Node v b1 b2 b3 b4 b5 b6 b7)
    = (f v) <> (foldMap f b1) <> (foldMap f b2) <> (foldMap f b3) <> (foldMap f b4)
            <> (foldMap f b5) <> (foldMap f b6) <> (foldMap f b7)
   
instance Show a => Show (GameTree a) where
  show Leaf = " Leaf "
  show (Node v b1 b2 b3 b4 b5 b6 b7) =
    " Node " ++ (show b1) ++ (show b2) ++ (show b3) ++ (show b4) ++ (show b5) ++ (show b6) ++ (show b7) 




