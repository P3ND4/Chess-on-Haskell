module Main where
  import Graphics.UI.Gtk
  import Control.Monad.IO.Class
  import GameStart(mainMenu)

  main :: IO ()
  main = do
    initGUI
    window <- windowNew
    set window [ windowTitle := "Chess", windowDefaultWidth := 1024
                , windowDefaultHeight := 720, containerBorderWidth := 0 ]
    mainScreen window
    widgetShowAll window
    mainGUI

  mainScreen :: Window -> IO ()
  mainScreen window = do

    eventBox <- eventBoxNew
    containerAdd window eventBox
    image <- imageNew
    imageSetFromFile image "./Background/BackG.jpg"
    containerAdd eventBox image

    eventBox `on` buttonPressEvent $ tryEvent $ do
      liftIO $ do 
        containerRemove window eventBox
        mainMenu window
      return ()

    widgetShowAll window
