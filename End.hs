module End where
    import Ficha
    import ActionFicha(all_valid_plays, all_plays)
    import Valid(jaque)
    import Table(todasLasFichasDe)
    import Aux(rival_color)

    --devuelve 0 si hay empate, 1 si perdio, 2 si puede jugar
    if_end :: [[Maybe Ficha]] -> Int -> Int
    if_end table turno = 
        if length (all_valid_plays table (todasLasFichasDe table turno)) == 0 then
            if jaque table turno (all_plays table (todasLasFichasDe table (rival_color turno))) then 1 else 0
        else 2


