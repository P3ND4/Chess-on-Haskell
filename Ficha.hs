module Ficha where
    
    --inicializacion de la estructura ficha
    data Ficha = Ficha {name :: String, pos :: (Int, Int), color :: Int, played :: Bool, dir :: String}

  