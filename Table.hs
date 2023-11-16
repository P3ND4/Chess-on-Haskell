module Table where
    import Ficha

    modifyTable :: [[Maybe Ficha]] -> Ficha -> (Int, Int) -> [[Maybe Ficha]]    
    modifyTable table ficha coord   | name ficha == "rey" && color ficha == 0 && (snd (pos ficha)) - (snd coord) == 2 =  modifyTableRec (modifyTableRec (modifyTable table (getFichaInPos table (7, 0))  (7, 3)) Nothing (pos ficha) 0) (makeNewFicha (Just ficha) coord) coord 0  
                                    | name ficha == "rey" && color ficha == 0 && (snd (pos ficha)) - (snd coord) == -2 =  modifyTableRec (modifyTableRec (modifyTable table (getFichaInPos table (7, 7))  (7, 5)) Nothing (pos ficha) 0) (makeNewFicha (Just ficha) coord) coord 0  
                                    | name ficha == "rey" && color ficha == 1 && (snd (pos ficha)) - (snd coord) == 2 =  modifyTableRec (modifyTableRec (modifyTable table (getFichaInPos table (0, 0))  (0, 3)) Nothing (pos ficha) 0) (makeNewFicha (Just ficha) coord) coord 0  
                                    | name ficha == "rey" && color ficha == 1 && (snd (pos ficha)) - (snd coord) == -2 =  modifyTableRec (modifyTableRec (modifyTable table (getFichaInPos table (0, 7))  (0, 5)) Nothing (pos ficha) 0) (makeNewFicha (Just ficha) coord) coord 0 
                                    | name ficha == "peon" && fst coord == 0 = modifyTableRec (modifyTableRec table Nothing (pos ficha) 0) (Just Ficha { dir = "./piezas/DamaB.png", pos = coord , played = True, color = 0, name = "dama" }) coord 0
                                    | name ficha == "peon" && fst coord == 7 = modifyTableRec (modifyTableRec table Nothing (pos ficha) 0) (Just Ficha { dir = "./piezas/DamaN.png", pos = coord , played = True, color = 1, name = "dama" }) coord 0
                                    | otherwise = modifyTableRec (modifyTableRec table Nothing (pos ficha) 0) (makeNewFicha (Just ficha) coord) coord 0
    getFichaInPos :: [[Maybe Ficha]] -> (Int, Int) -> Ficha
    getFichaInPos table (row, col) = 
        case (table!!row)!!col of
            Just f -> f                                                   
            Nothing -> Ficha { dir = "./piezas/PeonN.png", pos = (1,7), played = False, color = 1, name = "peon" }

    modifyTableRec :: [[Maybe Ficha]] -> Maybe Ficha -> (Int, Int) -> Int -> [[Maybe Ficha]]
    modifyTableRec [] _ _ _ = []
    modifyTableRec (f:tail) ficha (row , col) count | count == row = ([(modifyRow f ficha col 0)] ++ tail)
                                                    | otherwise =[f] ++ modifyTableRec tail ficha (row , col) (count + 1)

    modifyRow :: [Maybe Ficha] -> Maybe Ficha -> Int -> Int -> [Maybe Ficha]
    modifyRow [] _ _ _ = []
    modifyRow (f:tail) ficha col count | (count == col) = (ficha:tail)
                                       | otherwise = [f] ++ modifyRow tail ficha col (count + 1)
    makeNewFicha :: Maybe Ficha -> (Int , Int) -> Maybe Ficha
    makeNewFicha ficha coord = do
        case ficha of
            Just f -> (Just ficha) where ficha = Ficha { dir = (dir f), pos = coord, played = True, name = name f, color = color f}
            Nothing -> Nothing

    todasLasFichasDe :: [[Maybe Ficha]] -> Int -> [Ficha]
    todasLasFichasDe [] _ = []
    todasLasFichasDe (x:xs) color = (fichasFila x color) ++ (todasLasFichasDe xs color)

    fichasFila :: [Maybe Ficha] -> Int -> [Ficha]
    fichasFila [] _ = []
    fichasFila (x:xs) player = do
        case x of
            Just f | color f == player -> [f] ++ (fichasFila xs player)
                   | otherwise -> fichasFila xs player
            Nothing -> fichasFila xs player

    initTable :: [[Maybe Ficha]]
    initTable = [[Just torreN1, Just caballoN1, Just alfilN1, Just damaN, Just reyN, Just alfilN2, Just caballoN2, Just torreN2]
                 ,[Just peonN1, Just peonN2, Just peonN3, Just peonN4, Just peonN5, Just peonN6, Just peonN7, Just peonN8]
                 ,[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
                 ,[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
                 ,[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
                 ,[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
                 ,[Just peonB1, Just peonB2, Just peonB3, Just peonB4, Just peonB5, Just peonB6, Just peonB7, Just peonB8]
                 ,[Just torreB1, Just caballoB1, Just alfilB1, Just damaB, Just reyB, Just alfilB2, Just caballoB2, Just torreB2]
                 ]
                 where
                    
                    torreN1 = Ficha { dir = "./piezas/TorreN.png", pos = (0,0), played = False, color = 1, name = "torre" } 
                    caballoN1 = Ficha { dir = "./piezas/CaballoN.png", pos = (0,1), played = False, color = 1, name = "caballo" }
                    alfilN1 = Ficha { dir = "./piezas/AlfilN.png", pos = (0,2), played = False, color = 1, name = "alfil" }
                    damaN = Ficha { dir = "./piezas/DamaN.png", pos = (0,3), played = False, color = 1, name = "dama" }   
                    reyN = Ficha { dir = "./piezas/ReyN.png", pos = (0,4), played = False, color = 1, name = "rey" } 
                    alfilN2 = Ficha { dir = "./piezas/AlfilN.png", pos = (0,5), played = False, color = 1, name = "alfil" }
                    caballoN2 = Ficha { dir = "./piezas/CaballoN.png", pos = (0,6), played = False, color = 1, name = "caballo" }
                    torreN2 = Ficha { dir = "./piezas/TorreN.png", pos = (0,7), played = False, color = 1, name = "torre" } 
                    peonN1 = Ficha { dir = "./piezas/PeonN.png", pos = (1,0), played = False, color = 1, name = "peon" } 
                    peonN2 = Ficha { dir = "./piezas/PeonN.png", pos = (1,1), played = False, color = 1, name = "peon" } 
                    peonN3 = Ficha { dir = "./piezas/PeonN.png", pos = (1,2), played = False, color = 1, name = "peon" } 
                    peonN4 = Ficha { dir = "./piezas/PeonN.png", pos = (1,3), played = False, color = 1, name = "peon" } 
                    peonN5 = Ficha { dir = "./piezas/PeonN.png", pos = (1,4), played = False, color = 1, name = "peon" } 
                    peonN6 = Ficha { dir = "./piezas/PeonN.png", pos = (1,5), played = False, color = 1, name = "peon" } 
                    peonN7 = Ficha { dir = "./piezas/PeonN.png", pos = (1,6), played = False, color = 1, name = "peon" } 
                    peonN8 = Ficha { dir = "./piezas/PeonN.png", pos = (1,7), played = False, color = 1, name = "peon" } 
                    
                    peonB1 = Ficha { dir = "./piezas/PeonB.png", pos = (6,0) , played = False, color = 0, name = "peon" } 
                    peonB2 = Ficha { dir = "./piezas/PeonB.png", pos = (6,1) , played = False, color = 0, name = "peon" } 
                    peonB3 = Ficha { dir = "./piezas/PeonB.png", pos = (6,2) , played = False, color = 0, name = "peon" } 
                    peonB4 = Ficha { dir = "./piezas/PeonB.png", pos = (6,3) , played = False, color = 0, name = "peon" } 
                    peonB5 = Ficha { dir = "./piezas/PeonB.png", pos = (6,4) , played = False, color = 0, name = "peon" }
                    peonB6 = Ficha { dir = "./piezas/PeonB.png", pos = (6,5) , played = False, color = 0, name = "peon" } 
                    peonB7 = Ficha { dir = "./piezas/PeonB.png", pos = (6,6) , played = False, color = 0, name = "peon" } 
                    peonB8 = Ficha { dir = "./piezas/PeonB.png", pos = (6,7) , played = False, color = 0, name = "peon" }
                    torreB1 = Ficha { dir = "./piezas/TorreB.png", pos = (7,0) , played = False, color = 0, name = "torre" }
                    caballoB1 = Ficha { dir = "./piezas/CaballoB.png", pos = (7,1) , played = False, color = 0, name = "caballo" }
                    alfilB1 = Ficha { dir = "./piezas/AlfilB.png", pos = (7,2) , played = False, color = 0, name = "alfil" }
                    damaB = Ficha { dir = "./piezas/DamaB.png", pos = (7,3) , played = False, color = 0, name = "dama" }   
                    reyB = Ficha { dir = "./piezas/ReyB.png", pos = (7,4) , played = False, color = 0, name = "rey" }  
                    alfilB2 = Ficha { dir = "./piezas/AlfilB.png", pos = (7,5) , played = False, color = 0, name = "alfil" }
                    caballoB2 = Ficha { dir = "./piezas/CaballoB.png", pos = (7,6) , played = False, color = 0, name = "caballo" }
                    torreB2 = Ficha { dir = "./piezas/TorreB.png", pos = (7,7) , played = False, color = 0, name = "torre" } 
    
    --para saber si en una casilla hay una ficha 
    is_ficha :: [[Maybe Ficha]] -> (Int, Int) -> Bool
    is_ficha table (i, j) = do
        case ((table !! i) !! j) of
            Just f -> True
            Nothing -> False
    
    getKing :: [Ficha] -> (Int, Int)
    getKing (x:xs) | name x == "rey" = pos x 
                 | otherwise = getKing xs