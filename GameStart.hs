module GameStart where

  import Graphics.UI.Gtk
  import Control.Monad.IO.Class
  import Data.IORef
  import Control.Monad 
  import Control.Monad (when)
  import Table
  import Data.Maybe (fromJust)
  import Ficha
  import ActionFicha (valid_moves, all_plays)
  import Graphics.UI.Gtk.General.General
  import Data.Maybe (listToMaybe)
  import Data.Maybe (isJust)
  import System.IO.Unsafe (unsafePerformIO)
  import Data.Typeable
  import Data.Type.Equality
  import Graphics.UI.Gtk.Abstract.Widget
  import Valid
  import End
  import Aux
  import MinMax
 -- import Graphics.UI.Gtk.General.Enums (unsafeCastTo)

  gameStart :: Window -> Int -> IO ()
  gameStart window aI = do
 
    mainTable <- tableNew 10 9 True
    event <- eventBoxNew
    event2 <- eventBoxNew
    widgetModifyBg event2 StateNormal (Color 0 0 0)

    aspectFrame <- aspectFrameNew 0.5 0.5 (Just 1.0)
    
    tableAttachDefaults mainTable aspectFrame 0 9 0 9

    containerAdd window mainTable

    
    table <- tableNew 9 9 True
    selectionsRef <- newIORef []
    gameTable <- newIORef initTable


    label1 <- labelNew (Just "Turno de las Blancas")
    text <- labelGetText label1
    let modifiedText = "<span foreground='white' size='30000'>" ++ text ++ "</span>"
    labelSetMarkup label1 modifiedText
    tableAttachDefaults mainTable label1 2 8 9 10 

    eventBox <- eventBoxNew
    label <- labelNew (Just "Quit")
    containerAdd eventBox label
    tableAttachDefaults mainTable eventBox 0 2 9 10

    let color = Color 0 20000 0
    widgetModifyBg eventBox StateNormal color
    onDestroy window mainQuit
    eventBox `on` enterNotifyEvent $ do
        liftIO $ do
            widgetModifyBg eventBox StateNormal (Color 0 30000 0)
            return False
    
    eventBox `on` leaveNotifyEvent $ do
        liftIO $ do
            widgetModifyBg eventBox StateNormal color
            return False
    
    eventBox `on` buttonPressEvent $ tryEvent $ do
        liftIO $ do
            emergentWindow window
            


    mapM_ (\row -> mapM_ (\col -> createChessSquare table row col selectionsRef gameTable label1 aI) [0..7]) [0..7]
    mapM_ (\row -> mapM_ (\col -> createCoordLabel table row col) [8]) [0..8]
    mapM_ (\row -> mapM_ (\col -> createCoordLabel table row col) [0..8]) [8]
    containerAdd aspectFrame table
    onDestroy window mainQuit
    tableSetHomogeneous table True
    widgetShowAll window

 
  -- Función auxiliar para crear una caja de evento para cada casilla del tablero
  createChessSquare :: Table -> Int -> Int -> IORef[(Int, Int)] -> IORef [[Maybe Ficha]]-> Label-> Int -> IO ()
  createChessSquare table row col selectionsRef gameTable label aI = do
    eventBox <- eventBoxNew

    tableAttachDefaults table eventBox col (col + 1) row (row + 1)
    gameTableRef <- readIORef gameTable
    -- Establecer el color de fondo de la caja de evento según la posición 
    let color = if (row + col) `mod` 2 == 0 then Color 59535 59535 59535 else Color 29740 17375 6219
    widgetModifyBg eventBox StateNormal color
    let ficha = (gameTableRef!!row)!!col
    case ficha of
      Just f -> do
        let path = dir f
        image <- imageNew
        imageSetFromFile image path
        containerAdd eventBox image
      Nothing -> return ()

    widgetModifyBg eventBox StateNormal color

    -- Conectar una señal de clic a la caja de evento
    eventBox `on` buttonPressEvent $ tryEvent $ do
      liftIO $ do 
          --putStrLn $ "Haz clic en la casilla (" ++ show row ++ ", " ++ show col ++ ")"

          selections <- readIORef selectionsRef
          gameTableRef <- readIORef gameTable
          
          let turno | (length selections) `mod` 4 < 2 = 0
                    | otherwise = 1
          
          if (length selections) `mod` 2 == 1
              then do
                  let selectionnew = (row, col)
                  let oldSel = selections!!(length selections -1)
                  let pficha = (gameTableRef!!(fst oldSel))!!(snd oldSel)
                  if validPos gameTableRef pficha (row, col) then do
                    modifyIORef selectionsRef (\s -> s++[selectionnew])
                    clearColor table
                    moveImage table eventBox pficha selectionnew
                    case pficha of
                      Just fic  | (name fic) == "peon" && row == 0 -> do
                                  moveImage table eventBox (Just Ficha {name = "dama", pos = pos fic, played = False, color = turno, dir = "./piezas/DamaB.png"}) selectionnew
                                  let newtable = modifyTable gameTableRef fic (row ,col)
                                  modifyIORef gameTable (\x -> newtable)                                
                                
                                | (name fic) == "peon" && row == 7 -> do
                                  moveImage table eventBox (Just Ficha {name = "dama", pos = pos fic, played = False, color = turno, dir = "./piezas/DamaN.png"}) selectionnew
                                  let newtable = modifyTable gameTableRef fic (row ,col)
                                  modifyIORef gameTable (\x -> newtable)                                
                                
                                
                                | otherwise -> do 
                                  let newtable = modifyTable gameTableRef fic (row ,col)
                                  modifyIORef gameTable (\x -> newtable)
                      Nothing -> return ()
                    newTable <- readIORef gameTable
                    if ifEnroque newTable pficha col then do
                      case turno of
                        0 | (snd (selections !! (length selections -1))) - col == 2 -> moveImageEnroque table (7, 3) ((gameTableRef !! 7) !! 0)
                          | (snd (selections !! (length selections -1))) - col == -2 -> moveImageEnroque table (7, 5) ((gameTableRef !! 7) !! 7)
                        1 | (snd (selections !! (length selections -1))) - col == 2 -> moveImageEnroque table (0, 3) ((gameTableRef !! 0) !! 0)
                          | (snd (selections !! (length selections -1))) - col == -2 -> moveImageEnroque table (0, 5) ((gameTableRef !! 0) !! 7)
                      else return ()
                    
                    ifJaque table newTable (length selections `mod` 4)
                    
                    let text = endGame newTable (length selections `mod` 4)

                    let modifiedText = "<span foreground='white' size='30000'>" ++ text ++ "</span>"
                    
                    labelSetMarkup label modifiedText

                    if aI > 0 then do
                      let aiMove = playMinMax newTable aI
                      let aiGameTable = modifyTable newTable (fst aiMove) (snd aiMove)
                      modifyIORef gameTable (\x -> aiGameTable)
                      moveImageEnroque table (snd aiMove) (Just (fst aiMove)) 
                      modifyIORef selectionsRef (\x -> x ++ [pos (fst aiMove), (snd aiMove)])
                     else return ()

                  else return ()

              else do
                  let selectionnew = (row, col)
                  if fichaValida gameTableRef selectionnew (length selections `mod` 4) then do
                    modifyIORef selectionsRef (\s -> s++[selectionnew])
                    showMoves (row, col) table gameTableRef
                  else return ()
          

    return ()
  

  ifEnroque :: [[Maybe Ficha]] -> Maybe Ficha -> Int -> Bool
  ifEnroque table ficha col = 
    case ficha of 
      Just fic | (name fic) == "rey" && abs ((snd (pos fic)) - col) == 2 -> True 
               | otherwise -> False
      Nothing -> False


  endGame :: [[Maybe Ficha]] -> Int -> String
  endGame table modul =
    let mate = "Jaque Mate"
        tablas = "Tablas"
        final = if modul < 2 then if_end table 1 else if_end table 0
    in
      case final of
        0 ->  tablas
        1 ->  mate
        _ -> if modul < 2 then  "Turno de Negras" else  "Turno de Blancas"

  ifJaque :: Table -> [[Maybe Ficha]] -> Int -> IO ()
  ifJaque table gameTable modul = do
    let turn | modul < 2 = 0
             | otherwise = 1
    if jaque gameTable (rival_color turn) (all_plays gameTable (todasLasFichasDe gameTable turn)) then do
      colorBox table $ getKing $ todasLasFichasDe gameTable $ rival_color turn
      else return ()



  printFicha :: Maybe Ficha -> IO ()
  printFicha ficha = do
    case ficha of
      Just f -> putStrLn (name f)
      Nothing -> return ()

  validPos :: [[Maybe Ficha]] -> Maybe Ficha -> (Int ,Int) -> Bool
  validPos table ficha coord = 
    case ficha of
      Just f -> elem coord (valid_moves f table)
      Nothing -> False
                              

  createCoordLabel :: Table -> Int -> Int -> IO Label
  createCoordLabel table row col = do
    eventBox <- eventBoxNew
    let chars = ["A", "B", "C", "D", "E", "F", "G", "H"]
    label <- labelNew (Just " ")
    tableAttachDefaults table eventBox col (col + 1) row (row + 1)
    when (row == 8 && col /= 8 ) (labelSetText label (chars!!col))  
    when (col == 8 && row /= 8) (labelSetText label (show (8 - row)))
    widgetModifyBg eventBox StateNormal (Color 5000 5000 5000)
    containerAdd eventBox label

    text <- labelGetText label
    let modifiedText = "<span foreground='white' size='20000'>" ++ text ++ "</span>"
    labelSetMarkup label modifiedText

    return label
  
  fichaValida :: [[Maybe Ficha]] -> (Int , Int) -> Int -> Bool
  fichaValida table (row, col) turno =
    case (table!!row)!!col of
        Just f | turno > 1 && (color f == 0) -> False
               | turno < 2 && (color f == 1) -> False
               | (valid_moves f table) == [] -> False
               | otherwise -> True
        Nothing -> False



  mainMenu ::Window -> IO ()
  mainMenu window = do

        widgetModifyBg window StateNormal (Color 0 25535 0)   

        table <- tableNew 12 10 True
        containerAdd window table

        label <- labelNew (Just "Welcome to Pendulum Machi-san Chess")
        text <- labelGetText label
        let modifiedText = "<span foreground='white' size='30000'>" ++ text ++ "</span>"
        labelSetMarkup label modifiedText
        tableAttachDefaults table label 0 10 0 1 



        buttonPvP <- eventBoxNew
        labelPvP <- labelNew (Just "Player vs Player")
        containerAdd buttonPvP labelPvP

        buttonPvE <- eventBoxNew
        labelPvE <- labelNew (Just "Player vs PC")
        containerAdd buttonPvE labelPvE

        buttonQuit <- eventBoxNew
        labelQuit <- labelNew (Just "Quit")
        containerAdd buttonQuit labelQuit       
       
        -- accion de quit
        tableAttachDefaults table buttonQuit 4 6 10 11 
        let color = Color 0 20000 0
        widgetModifyBg buttonQuit StateNormal color
        
        buttonQuit `on` enterNotifyEvent $ do
            liftIO $ do
                widgetModifyBg buttonQuit StateNormal (Color 0 30000 0)
                return False
        
        buttonQuit `on` leaveNotifyEvent $ do
            liftIO $ do
                widgetModifyBg buttonQuit StateNormal color
                return False


        buttonQuit `on` buttonPressEvent $ tryEvent $ do
            liftIO mainQuit
       
       
        --acciones del PvE
        tableAttachDefaults table buttonPvE 4 6 5 6 
        widgetModifyBg buttonPvE StateNormal color
        
        buttonPvE `on` enterNotifyEvent $ do
            liftIO $ do
                widgetModifyBg buttonPvE StateNormal (Color 0 30000 0)
                return False
        
        buttonPvE `on` leaveNotifyEvent $ do
            liftIO $ do
                widgetModifyBg buttonPvE StateNormal color
                return False


        buttonPvE `on` buttonPressEvent $ tryEvent $ do
            liftIO $ do             
                containerRemove window table
                aiLvlSelector window 


        --acciones del boton PvP
        tableAttachDefaults table buttonPvP 4 6 3 4 
        widgetModifyBg buttonPvP StateNormal color
        
        buttonPvP `on` enterNotifyEvent $ do
            liftIO $ do
                widgetModifyBg buttonPvP StateNormal (Color 0 30000 0)
                return False
        
        buttonPvP `on` leaveNotifyEvent $ do
            liftIO $ do
                widgetModifyBg buttonPvP StateNormal color
                return False


        buttonPvP `on` buttonPressEvent $ tryEvent $ do
            liftIO $ do             
                containerRemove window table
                gameStart window 0
        
        onDestroy window mainQuit

        widgetShowAll window

  aiLvlSelector :: Window -> IO () 
  aiLvlSelector window = do
        table <- tableNew 14 10 True
        containerAdd window table

        mapM_ (\row -> buildBox window table row) [3, 5, 7, 9]

        eventBox <- eventBoxNew
        label <- labelNew (Just "Back")
        containerAdd eventBox label
        
        tableAttachDefaults table eventBox 4 6 11 12

        let color = Color 0 20000 0
        widgetModifyBg eventBox StateNormal color
        
        eventBox `on` enterNotifyEvent $ do
            liftIO $ do
                widgetModifyBg eventBox StateNormal (Color 0 30000 0)
                return False
        
        eventBox `on` leaveNotifyEvent $ do
            liftIO $ do
                widgetModifyBg eventBox StateNormal color
                return False
        
        eventBox `on` buttonPressEvent $ tryEvent $ do
            liftIO $ do
                containerRemove window table
                mainMenu window





        widgetShowAll window

    



  buildBox :: Window -> Table -> Int -> IO EventBox
  buildBox window table row = do
        let dificult = ["Easy", "Medium", "Hard", "Very Hard"]
        let lvl = (row - 3) `div` 2
        eventBox <- eventBoxNew
        tableAttachDefaults table eventBox 4 6 row (row +1)
        label <- labelNew (Just (dificult!!lvl))
        containerAdd eventBox label
    
        let color = Color 0 20000 0
        widgetModifyBg eventBox StateNormal color
        
        eventBox `on` enterNotifyEvent $ do
            liftIO $ do
                widgetModifyBg eventBox StateNormal (Color 0 30000 0)
                return False
        
        eventBox `on` leaveNotifyEvent $ do
            liftIO $ do
                widgetModifyBg eventBox StateNormal color
                return False
        
        eventBox `on` buttonPressEvent $ tryEvent $ do
            liftIO $ do
              containerRemove window table
              gameStart window ((row-1)`div`2)

        return eventBox

  emergentWindow :: Window -> IO ()
  emergentWindow mainWindow = do
    initGUI
    window <- windowNew
    set window [ windowTitle := " ", windowDefaultWidth := 200
                , windowDefaultHeight := 100, containerBorderWidth := 10 ]

    children <- containerGetChildren mainWindow

    table <- tableNew 2 2 True
    label <- labelNew (Just "Are you sure?")
    tableAttachDefaults table label 0 2 0 1
    containerAdd window table
    yes <- buttonNewWithLabel "yes"
    tableAttachDefaults table yes 0 1 1 2
    onClicked yes $ do
      containerRemove mainWindow (children!!0)
      mainMenu mainWindow
      widgetDestroy window


    no <- buttonNewWithLabel "no"
    tableAttachDefaults table no 1 2 1 2
    onClicked no $ do
      widgetDestroy window

    widgetShowAll window
    mainGUI

  showMoves :: (Int, Int) -> Table ->  [[Maybe Ficha]]-> IO ()
  showMoves (row, col) table gameTable = do
    case (gameTable!!row)!!col of
        Just ficha -> do
            let coords = valid_moves ficha gameTable
            mapM_ (\coord -> colorBox table coord) coords
        Nothing -> return ()

  colorBox :: Table -> (Int, Int) -> IO ()
  colorBox table (row,col) = do
      children <- containerGetChildren table
      let eventBoxes = reverse children
      let pos = (row *8 + col)
      let child = listToMaybe (drop pos eventBoxes)
      case child of
          Just box | (row + col) `mod` 2 == 1 -> widgetModifyBg box StateNormal (Color 30000 0 0)
                   | otherwise -> widgetModifyBg box StateNormal (Color 60000 10000 10000)
          Nothing -> return ()
  

  esCajaEventos :: Typeable widget => widget -> IO Bool
  esCajaEventos widget = do
    box <- eventBoxNew
    if sonDelMismoTipo box widget then return True else return False

  sonDelMismoTipo :: (Typeable a, Typeable b) => a -> b -> Bool
  sonDelMismoTipo x y = typeOf x == typeOf y

  clearColor :: Table -> IO ()
  clearColor table = do
    children <- containerGetChildren table
    let boxes = reverse children
    mapM_ (\x -> setNaturalColor boxes x) [0..63]
  
  setNaturalColor :: [Widget] -> Int-> IO ()
  setNaturalColor box pos = if (row + col) `mod` 2 == 0 then widgetModifyBg (box!!pos) StateNormal (Color 59535 59535 59535) else widgetModifyBg (box!!pos) StateNormal (Color 29740 17375 6219)
    where
      row = pos `div` 8
      col = pos `mod` 8
                
  
  moveImage :: Table -> EventBox -> Maybe Ficha -> (Int, Int) -> IO ()
  moveImage table box ori dest = do
    case ori of
      Just ficha -> do
        removeImage table ((fst(pos ficha)) * 8 + (snd (pos ficha)))
        placeImage box ficha
      Nothing -> return ()
    
  

  removeImage :: Table -> Int -> IO ()
  removeImage table pos = do
    children <- containerGetChildren table
    let boxes = reverse children
    boxChild <- containerGetChildren (castToEventBox (boxes!!pos)) 
    if length boxChild > 0 then do
      containerRemove (castToEventBox (boxes!!pos)) (boxChild!!0)
    else return ()
  
  placeImage :: EventBox -> Ficha -> IO ()
  placeImage eventBox ficha = do
    image <- imageNew
    imageSetFromFile image (dir ficha)
    boxChild <- containerGetChildren eventBox
    if length boxChild > 0 then do
      containerRemove eventBox (boxChild!!0)
      containerAdd eventBox image
    else containerAdd eventBox image
    widgetShowAll eventBox

  moveImageEnroque :: Table -> (Int, Int) -> Maybe Ficha -> IO ()
  moveImageEnroque table (row, col) torre = do
    putStrLn "entra"
    
    children <- containerGetChildren table
    let boxes = reverse children
    case torre of
      Just tower -> do
        removeImage table ((fst(pos tower)) * 8 + (snd (pos tower)))
        placeImage (castToEventBox (boxes !! (row * 8 + col))) tower
      Nothing -> return()
