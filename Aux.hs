module Aux where
    import Ficha

    --redefinicion de la funcion filter para aplicar una funcion con mas de un parametro
    my_filter :: (Ficha -> [[Maybe Ficha]] -> a -> Bool) -> Ficha -> [[Maybe Ficha]] -> [a] -> [a]
    my_filter function ficha table [] = []
    my_filter function ficha table xs = [x| x <- xs, function ficha table x] 

    --color de las fichas del rival
    rival_color :: Int -> Int
    rival_color color_ficha = if color_ficha == 1 then 0 else 1
   
    --distancia entre 2 tuplas en la misma fila
    distance :: (Int, Int) -> (Int, Int) -> Int
    distance (i, j) (x, y) = abs (j - y)