module MinMax where
    import Ficha
    import ActionFicha
    import Table
    import End
    import Valid
    import Aux
    import PositionValues
    import Control.Monad.IO.Class
    import System.IO.Unsafe (unsafePerformIO)        

    playMinMax :: [[Maybe Ficha]] -> Int -> (Ficha, (Int, Int))
    playMinMax table dificult = fst (bestMove table (getAllMoves table (todasLasFichasDe table 1)) dificult)
    
    bestMove :: [[Maybe Ficha]] -> [(Ficha, (Int, Int))] -> Int -> ((Ficha, (Int, Int)), Int)
    bestMove table [x] maxLvl = (x, min)
        where
            newTable = modifyTable table (fst x) (snd x)
            min = playMin newTable (getAllMoves newTable (todasLasFichasDe newTable 0)) maxLvl 1 (-1000) 1000
    bestMove table (x:xs) maxLvl | min > snd nexValue = (x, min)
                                 | otherwise = nexValue
        where
            newTable = modifyTable table (fst x) (snd x)
            min = playMin newTable (getAllMoves newTable (todasLasFichasDe newTable 0)) maxLvl 1 (-1000) 1000
            nexValue = bestMove table xs maxLvl
    
    playMax :: [[Maybe Ficha]] -> [(Ficha, (Int, Int))] -> Int -> Int -> Int -> Int -> Int 
    playMax table [x] maxLvl current alpha beta | maxLvl == current = muvesValueMax table [x] 0
                                                | if_end newTable 0 == 1 = 1000
                                                | alpha >= beta = (-1000)
                                                | otherwise = playMin newTable (getAllMoves newTable (todasLasFichasDe newTable 0)) maxLvl (current + 1) alpha beta
        where newTable = modifyTable table (fst x) (snd x)
    playMax table (x:xs) maxLvl current alpha beta | maxLvl == current = muvesValueMax table (x:xs) 0
                                                   | if_end newTable 0 == 1 = 1000
                                                   | alpha >= beta = (-1000)
                                                   | minResult < nextChild = nextChild
                                                   | minResult > nextChild = minResult
        where
            newTable = modifyTable table (fst x) (snd x)
            minResult = playMin newTable (getAllMoves newTable (todasLasFichasDe newTable 0)) maxLvl (current + 1) alpha beta
            nextChild | minResult > beta = playMax table xs maxLvl current minResult beta 
                      | otherwise = playMax table xs maxLvl current alpha beta



    playMin :: [[Maybe Ficha]] -> [(Ficha, (Int, Int))] -> Int -> Int -> Int -> Int -> Int
    playMin table [x] maxLvl current alpha beta | maxLvl == current = muvesValueMin table [x] 0
                                                | if_end newTable 1 == 1 = (-1000)
                                                | alpha >= beta = 1000 
                                                | otherwise = playMax newTable (getAllMoves newTable (todasLasFichasDe newTable 1)) maxLvl (current + 1) alpha beta
        where newTable = modifyTable table (fst x) (snd x)
    playMin table (x:xs) maxLvl current alpha beta | maxLvl == current = muvesValueMax table (x:xs) 0
                                                   | if_end newTable 1 == 1 = (-1000)
                                                   | alpha >= beta = 1000
                                                   | maxResult > nextChild = nextChild
                                                   | maxResult < nextChild = maxResult
        where
            newTable = modifyTable table (fst x) (snd x)
            maxResult = playMax newTable (getAllMoves newTable (todasLasFichasDe newTable 1)) maxLvl (current + 1) alpha beta
            nextChild | maxResult < beta = playMin table xs maxLvl current alpha maxResult
                      | otherwise = playMin table xs maxLvl current alpha beta


    getAllMoves :: [[Maybe Ficha]] -> [Ficha] -> [(Ficha, (Int, Int))]
    getAllMoves _ [] = []
    getAllMoves table (x:xs) = (allMoves x (valid_moves x table)) ++ getAllMoves table xs

    allMoves :: Ficha -> [(Int, Int)] -> [(Ficha, (Int, Int))]
    allMoves _ [] = []
    allMoves ficha (x:xs) = [(ficha, x)] ++ allMoves ficha xs

    
    tableValue :: [[Maybe Ficha]] -> Int -> Int
    tableValue table turn   | turn == 0 && end == 1 = 1000
                            | turn == 1 && end == 1 = (-1000)
                            | otherwise = (playerValue (todasLasFichasDe table 1)) - (playerValue (todasLasFichasDe table 0))
            where
                end = if_end table turn


    muvesValueMax :: [[Maybe Ficha]] -> [(Ficha, (Int, Int))] -> Int -> Int
    muvesValueMax table [x] nextTurn = tableValue newTable nextTurn
            where newTable = modifyTable table (fst x) (snd x)
    muvesValueMax table (x:xs) nextTurn  | value > nexValue = value
                                         | otherwise = nexValue
            where
                newTable = modifyTable table (fst x) (snd x)
                value = tableValue newTable nextTurn
                nexValue = muvesValueMax table xs nextTurn
 
    muvesValueMin :: [[Maybe Ficha]] -> [(Ficha, (Int, Int))] -> Int -> Int
    muvesValueMin table [x] nextTurn = tableValue newTable nextTurn
            where newTable = modifyTable table (fst x) (snd x)
    muvesValueMin table (x:xs) nextTurn  | value < nexValue = value
                                         | otherwise = nexValue
            where
                newTable = modifyTable table (fst x) (snd x)
                value = tableValue newTable nextTurn
                nexValue = muvesValueMin table xs nextTurn

    playerValue :: [Ficha] -> Int
    playerValue [] = 0
    playerValue (x:xs) = (peso x) + (playerValue xs)


    peso :: Ficha -> Int
    peso ficha | name ficha == "peon" = pawnValues ficha
               | name ficha == "caballo" = knightValues ficha
               | name ficha == "torre" = rookValues ficha
               | name ficha == "alfil" = bishopValues ficha
               | name ficha == "dama" = queenValues ficha
               | otherwise = kingValuesMidgame ficha
    
