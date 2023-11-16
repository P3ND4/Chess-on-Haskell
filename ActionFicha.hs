module ActionFicha where
    import Ficha
    import Valid(is_valid, inside, jaque)
    import Aux
    import Table(modifyTable, todasLasFichasDe, is_ficha)
    
    --movimientos finales de cada ficha
    valid_moves :: Ficha -> [[Maybe Ficha]] -> [(Int, Int)]
    valid_moves ficha table | name ficha == "rey" = (moves_jaque ficha table (moves ficha table)) ++ enroque ficha table
                            | otherwise = moves_jaque ficha table (moves ficha table)

    --posibles jugadas de cada ficha
    moves :: Ficha -> [[Maybe Ficha]] -> [(Int, Int)]
    moves ficha table   | name ficha == "rey" = my_filter is_valid ficha table (moves_rey (pos ficha)) 
                        | name ficha == "caballo" = my_filter is_valid ficha table (moves_caballo (pos ficha))
                        | name ficha == "alfil" = my_filter is_valid ficha table (moves_alfil table (pos ficha) (color ficha))
                        | name ficha == "torre" = my_filter is_valid ficha table (moves_torre table (pos ficha) (color ficha))
                        | name ficha == "dama" = my_filter is_valid ficha table ((moves_alfil table (pos ficha) (color ficha)) ++ (moves_torre table (pos ficha) (color ficha)))
                        | otherwise = my_filter is_valid ficha table (moves_peon (pos ficha) table (color ficha) (played ficha))

    --posibles jugadas del rey
    moves_rey :: (Int, Int) -> [(Int, Int)]
    moves_rey (i, j) =  [(i, j + 1), (i, j - 1), (i + 1, j), (i -1, j), (i + 1, j + 1), (i + 1, j - 1), (i - 1, j + 1), (i - 1, j - 1)]

    --posibles jugadas del caballo
    moves_caballo :: (Int, Int) -> [(Int, Int)]
    moves_caballo (i, j) = [(i + 2, j + 1), (i + 2, j - 1), (i - 2, j + 1), (i - 2, j - 1), (i + 1, j + 2), (i + 1, j - 2), (i - 1, j + 2), (i - 1, j - 2)]

    --posibles jugadas del arfil
    moves_alfil :: [[Maybe Ficha]] -> (Int, Int) -> Int -> [(Int, Int)]
    moves_alfil table (i, j) color_ficha = (moves_rec table (i, j) color_ficha 1 (1, 1)) ++ (moves_rec table (i, j) color_ficha 1 (1, -1)) ++ (moves_rec table (i, j) color_ficha 1 (-1, 1)) ++ (moves_rec table (i, j) color_ficha 1 (-1, -1))

    --posibles jugadas de la torre
    moves_torre :: [[Maybe Ficha]] -> (Int, Int) -> Int -> [(Int, Int)]
    moves_torre table (i, j) color_ficha = (moves_rec table (i, j) color_ficha 1 (0, 1)) ++ (moves_rec table (i, j) color_ficha 1 (0, -1)) ++ (moves_rec table (i, j) color_ficha 1 (-1, 0)) ++ (moves_rec table (i, j) color_ficha 1 (1, 0))

    --metodo auxiliar para analizar las fichas que reciben una coordenada de direcciones
    moves_rec :: [[Maybe Ficha]] -> (Int, Int) -> Int -> Int -> (Int, Int) -> [(Int, Int)]
    moves_rec table (i, j) _ k (di, dj) |  not (inside (i + k * di, j + k * dj)) = []
    moves_rec table (i, j) color_ficha k (di, dj) = do
        case ((table !! (i + k * di)) !! (j + k * dj)) of
            Just f  | color f == color_ficha -> []
                    | otherwise ->  [(i + k * di, j + k *dj)]
            Nothing -> [(i + k * di, j + k * dj)] ++ moves_rec table (i,j) color_ficha (k+1) (di,dj)

    --movimientos del peon
    moves_peon :: (Int, Int) -> [[Maybe Ficha]] -> Int -> Bool -> [(Int, Int)]
    moves_peon (i, j) table color_ficha if_played = (normal_moves_peon table (i, j) color_ficha if_played) ++ (eating_moves_peon (i, j) table color_ficha)

    --paso del peon (1 paso si ya se movio, 1 o 2 en caso contrario)
    normal_moves_peon :: [[Maybe Ficha]] -> (Int, Int) -> Int -> Bool -> [(Int, Int)]
    normal_moves_peon table (i, j) color_ficha if_played    | if_played = if color_ficha == 0 then (if is_ficha table (i - 1, j) then [] else [(i - 1, j)]) else (if is_ficha table (i + 1, j) then [] else [(i + 1, j)])                          
                                                            | otherwise = if color_ficha == 0 then normal_moves_peon_white table (i, j) else normal_moves_peon_black table (i, j)

    --avance del peon blanco
    normal_moves_peon_white :: [[Maybe Ficha]] -> (Int, Int) -> [(Int, Int)]
    normal_moves_peon_white table (i, j) 
        | not(is_ficha table (i - 1, j)) && not(is_ficha table (i - 2, j)) = [(i - 1, j),(i - 2, j)]
        | not(is_ficha table (i - 1, j)) && is_ficha table (i - 2, j) = [(i - 1, j)]
        | otherwise = []

    --avance del peon negro
    normal_moves_peon_black :: [[Maybe Ficha]] -> (Int, Int) -> [(Int, Int)]
    normal_moves_peon_black table (i, j) 
        | not(is_ficha table (i + 1, j)) && not(is_ficha table (i + 2, j)) = [(i + 1, j),(i + 2, j)]
        | not(is_ficha table (i + 1, j)) && is_ficha table (i + 2, j) = [(i + 1, j)]
        | otherwise = []

    --movimientos diagonales del peon al comer
    eating_moves_peon :: (Int, Int) -> [[Maybe Ficha]] -> Int -> [(Int, Int)]
    eating_moves_peon (i, j) table color_ficha  | color_ficha == 0 = (eating_moves_peon_auxiliar (i - 1, j + 1) table color_ficha) ++ (eating_moves_peon_auxiliar (i - 1, j - 1) table color_ficha)
                                                | otherwise = (eating_moves_peon_auxiliar (i + 1, j + 1) table color_ficha) ++ (eating_moves_peon_auxiliar (i + 1, j - 1) table color_ficha)
    
    eating_moves_peon_auxiliar :: (Int, Int) -> [[Maybe Ficha]] -> Int -> [(Int, Int)]
    eating_moves_peon_auxiliar (dest_i, dest_j) table color_ficha = if not (inside (dest_i, dest_j)) then [] else do
        case ((table !! dest_i) !! dest_j) of
            Just f -> [(dest_i, dest_j)]
            Nothing -> []

    --movimientos usando la condicion de jaque
    moves_jaque :: Ficha -> [[Maybe Ficha]] ->[(Int, Int)] -> [(Int, Int)]
    moves_jaque ficha table [] = []
    moves_jaque ficha table (x:xs)  | not(jaque (modifyTable table ficha x) (color ficha) (all_plays (modifyTable table ficha x) (todasLasFichasDe (modifyTable table ficha x) (rival_color (color ficha))))) = x:(moves_jaque ficha table xs)
                                    | otherwise = moves_jaque ficha table xs

    --todas las posibles jugadas de un jugador ignorando la condicion de jaque
    all_plays :: [[Maybe Ficha]] -> [Ficha] -> [(Int, Int)]
    all_plays table [] = []
    all_plays table (x:xs) = moves x table ++ all_plays table xs  

    --todas las posibles jugadas de un jugador
    all_valid_plays :: [[Maybe Ficha]] -> [Ficha] -> [(Int, Int)]
    all_valid_plays table [] = []
    all_valid_plays table (x:xs) = valid_moves x table ++ all_valid_plays table xs

    --enroque 
    enroque :: Ficha -> [[Maybe Ficha]] -> [(Int, Int)]
    enroque rey table  = 
        if not(played rey) && not (jaque table (color rey) (all_plays table (todasLasFichasDe table (rival_color (color rey))))) && length torres > 0 then enroque_aux_moves rey table (path_rey_torre (color rey) torres) else []
                where  torres = get_torre (todasLasFichasDe table (color rey))

    --posicion del rey en caso de poder enrocar con una torre
    enroque_aux_move :: Ficha -> [[Maybe Ficha]] -> [(Int, Int)] -> Maybe (Int, Int)
    enroque_aux_move rey table dests    | can_enroque rey table dests && length dests == 2 = Just (last dests)
                                        | can_enroque rey table dests  && length dests == 3 = Just (dests !! 1)
                                        | otherwise = Nothing


    --posiciones del rey en caso de enrocar con cualquier torre que no se haya movido
    enroque_aux_moves :: Ficha -> [[Maybe Ficha]] -> [[(Int, Int)]] -> [(Int, Int)]
    enroque_aux_moves rey table [] = []
    enroque_aux_moves rey table (x:xs) = do
        case enroque_aux_move rey table x of
            Just mov -> [mov] ++ enroque_aux_moves rey table xs
            Nothing -> enroque_aux_moves rey table xs
        
    --torres que no se han movido    
    get_torre :: [Ficha] -> [Ficha]
    get_torre [] = []
    get_torre (x:xs)    | name x == "torre" && played x == False = x : (get_torre xs)
                        | otherwise = get_torre xs
    
    --casillas entre el rey y las torres
    path_rey_torre :: Int -> [Ficha] -> [[(Int, Int)]]
    path_rey_torre color_rey torres | color_rey == 1 = map path_black (map pos torres) 
                                    | otherwise = map path_white (map pos torres)
    
    --casillas entre el rey negro y una torre
    path_black :: (Int, Int) -> [(Int, Int)]
    path_black (i, j) = if distance (0, 4) (i, j) == 3 then [ (0, 5), (0, 6)] else [(0, 1), (0, 2), (0, 3)]

    --casillas entre el rey blanco y una torre
    path_white :: (Int, Int) -> [(Int, Int)]
    path_white (i, j) = if distance (7, 4) (i, j) == 3 then [(7, 5), (7, 6)] else [(7, 1), (7, 2), (7, 3)]

    --si se puede o no enrocar
    can_enroque :: Ficha -> [[Maybe Ficha]] -> [(Int, Int)] -> Bool
    can_enroque rey table [] = True
    can_enroque rey table (x:xs)    | length (x:xs) == 3 = can_enroque rey table xs
                                    | not(is_ficha table x) && not(jaque (modifyTable table rey x) (color rey) (all_plays (modifyTable table rey x) (todasLasFichasDe (modifyTable table rey x) (rival_color (color rey))))) = can_enroque rey table xs
                                    | otherwise = False
                                   
    
    