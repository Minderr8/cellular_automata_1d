import System.Environment (getArgs)
import Text.Read (readMaybe)
import Automata

main :: IO ()
main = do
  args <- getArgs
  case args of
    [r, a, g] ->
      case (readMaybe r, readMaybe a, readMaybe g) of
        (Just regla, Just ancho, Just gens)
          | regla >= 0 && regla <= 255 && ancho > 0 && gens > 0 -> do
                let a0 = inicial regla ancho
                    historia = take gens (iterate paso a0)
                mapM_ (putStrLn . dibujar) historia
        _ -> putStrLn "error: regla debe estar entre 0 y 255; ancho y generaciones deben ser mayores que 0"
    _ -> putStrLn "uso: ./prog <regla> <ancho> <generaciones>"
