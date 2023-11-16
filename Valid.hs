module Valid where
        import Ficha
        import Table
        
        --para saber si la posicion esta dentro de los limites del tablero
        inside :: (Int, Int) -> Bool
        inside (i, j) = elem i [0..7] && elem j [0..7]

        --saber si una jugada es valida
        is_valid :: Ficha -> [[Maybe Ficha]] -> (Int, Int) -> Bool
        is_valid ficha table (i, j) = inside (i, j) && ficha_or_nothing table ficha (i, j)
        
        --si en una posicion hay una ficha enemiga o si no hay nada 
        ficha_or_nothing :: [[Maybe Ficha]] -> Ficha -> (Int, Int) -> Bool
        ficha_or_nothing table ficha (i, j) = do
                case ((table !! i) !! j) of
                        Just f -> color f /= color ficha
                        Nothing -> True
        
        --saber si un jugador esta en jaque
        jaque :: [[Maybe Ficha]] -> Int -> [(Int, Int)] -> Bool
        jaque table turn [] = False
        jaque table turn ((i, j): xs) = do 
                case (table !! i) !! j of
                        Just f  | color f == turn && name f == "rey" -> True
                                | otherwise -> jaque table turn xs
                        Nothing -> jaque table turn xs