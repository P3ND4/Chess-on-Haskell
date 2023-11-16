module PositionValues where
  import Ficha


  pawnValues :: Ficha -> Int
  pawnValues ficha | color ficha == 0 = (pawnValuesB!!row)!!col
                   | otherwise = ((reverse pawnValuesB)!!row)!!col
        where
          row = fst $ pos ficha
          col = snd $ pos ficha

  pawnValuesB :: [[Int]] 
  pawnValuesB = [
    [0, 0, 0, 0, 0, 0, 0, 0],
    [50, 50, 50, 50, 50, 50, 50, 50],
    [10, 10, 20, 30, 30, 20, 10, 10],
    [5, 5, 10, 25, 25, 10, 5, 5],
    [0, 0, 0, 20, 20, 0, 0, 0],
    [5, -5, -10, 0, 0, -10, -5, 5],
    [5, 10, 10, -20, -20, 10, 10, 5],
    [0, 0, 0, 0, 0, 0, 0, 0]]



  knightValues :: Ficha -> Int
  knightValues ficha | color ficha == 0 = (knightValuesB!!row)!!col
                     | otherwise = ((reverse knightValuesB)!!row)!!col
        where
          row = fst $ pos ficha
          col = snd $ pos ficha

  knightValuesB :: [[Int]]
  knightValuesB = [
    [-50, -40, -30, -30, -30, -30, -40, -50],
    [-40, -20, 0, 0, 0, 0, -20, -40],
    [-30, 0, 10, 15, 15, 10, 0, -30],
    [-30, 5, 15, 20, 20, 15, 5, -30],
    [-30, 0, 15, 20, 20, 15, 0, -30],
    [-30, 5, 10, 15, 15, 10, 5, -30],
    [-40, -20, 0, 5, 5, 0, -20, -40],
    [-50, -40, -30, -30, -30, -30, -40, -50]
    ]

  bishopValues :: Ficha -> Int
  bishopValues ficha | color ficha == 0 = (bishopValuesB!!row)!!col
                     | otherwise = ((reverse bishopValuesB)!!row)!!col
        where
          row = fst $ pos ficha
          col = snd $ pos ficha


  bishopValuesB :: [[Int]]
  bishopValuesB = [
    [-20, -10, -10, -10, -10, -10, -10, -20],
    [-10, 0, 0, 0, 0, 0, 0, -10],
    [-10, 0, 5, 10, 10, 5, 0, -10],
    [-10, 5, 5, 10, 10, 5, 5, -10],
    [-10, 0, 10, 10, 10, 10, 0, -10],
    [-10, 10, 10, 10, 10, 10, 10, -10],
    [-10, 5, 0, 0, 0, 0, 5, -10],
    [-20, -10, -10, -10, -10, -10, -10, -20]
    ]

  rookValues :: Ficha -> Int
  rookValues ficha | color ficha == 0 = (rookValuesB!!row)!!col
                    | otherwise = ((reverse rookValuesB)!!row)!!col
        where
          row = fst $ pos ficha
          col = snd $ pos ficha

  rookValuesB :: [[Int]]
  rookValuesB = [
    [0, 0, 0, 0, 0, 0, 0, 0],
    [5, 10, 10, 10, 10, 10, 10, 5],
    [-5, 0, 0, 0, 0, 0, 0, -5],
    [-5, 0, 0, 0, 0, 0, 0, -5],
    [-5, 0, 0, 0, 0, 0, 0, -5],
    [-5, 0, 0, 0, 0, 0, 0, -5],
    [-5, 0, 0, 0, 0, 0, 0, -5],
    [0, 0, 0, 5, 5, 0, 0, 0]
    ]

  queenValues :: Ficha -> Int
  queenValues ficha | color ficha == 0 = (queenValuesB!!row)!!col
                    | otherwise = ((reverse queenValuesB)!!row)!!col
        where
          row = fst $ pos ficha
          col = snd $ pos ficha

  queenValuesB :: [[Int]]
  queenValuesB = [
    [-20, -10, -10, -5, -5, -10, -10, -20],
    [-10, 0, 0, 0, 0, 0, 0, -10],
    [-10, 0, 5, 5, 5, 5, 0, -10],
    [-5, 0, 5, 5, 5, 5, 0, -5],
    [0, 0, 5, 5, 5, 5, 0, -5],
    [-10, 5, 5, 5, 5, 5, 0, -10],
    [-10, 0, 5, 0, 0, 0, 0, -10],
    [-20, -10, -10, -5, -5, -10, -10, -20]
    ]

  kingValuesMidgame :: Ficha -> Int
  kingValuesMidgame ficha | color ficha == 0 = (kingValuesMidgameB!!row)!!col
                          | otherwise = ((reverse kingValuesMidgameB)!!row)!!col
        where
          row = fst $ pos ficha
          col = snd $ pos ficha

  kingValuesMidgameB :: [[Int]]
  kingValuesMidgameB = [
    [-30, -40, -40, -50, -50, -40, -40, -30],
    [-30, -40, -40, -50, -50, -40, -40, -30],
    [-30, -40, -40, -50, -50, -40, -40, -30],
    [-30, -40, -40, -50, -50, -40, -40, -30],
    [-20, -30, -30, -40, -40, -30, -30, -20],
    [-10, -20, -20, -20, -20, -20, -20, -10],
    [20, 20, 0, 0, 0, 0, 20, 20],
    [20, 30, 10, 0, 0, 10, 30, 20]
    ]

