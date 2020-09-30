--import Control.Monad  
--import Data.Char 
--main = forever $ do  
--    putStr "Give me some input: "  
--    l <- getLine  
--    putStrLn $ map toUpper l

import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL

main = do
    (_progName, _args) <- getArgsAndInitialize
    _window <- createWindow "Hello World"
    displayCallback $= display
    mainLoop
 
    --putStrLn "someFunc"

display :: DisplayCallback
display = do
    clear [ ColorBuffer ]
    renderPrimitive Points $
        mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) myPoints    
    flush

myPoints :: [(GLfloat,GLfloat,GLfloat)]
myPoints = [ (sin (2*pi*k/12), cos (2*pi*k/12), 0) | k <- [1..12] ]
















