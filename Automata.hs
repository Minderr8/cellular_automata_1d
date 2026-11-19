module Automata where

-- convertir numero a la regla
cajones :: Int -> [Bool]
cajones regla = digits 8 regla

digits :: Int -> Int -> [Bool]
digits 0 _ = []
digits i r = liveOrDead r : digits (i-1) (r `div` 2)

liveOrDead :: Int -> Bool
liveOrDead r = r `mod` 2 == 1

nuevoValor :: [Bool] -> (Bool, Bool, Bool) -> Bool
nuevoValor tabla (izq, mid, der) = let i = fromEnum izq
				       m = fromEnum mid
				       d = fromEnum der
				   in tabla !! ((4*i) + (2*m) + d) 

data Automata = Automata
  { celdas     :: [Bool]
  , regla      :: Int
  , generacion :: Int
  } deriving Show

paso :: Automata -> Automata
paso a = a { celdas = nuevas, generacion = generacion a + 1 }
  where
    tabla       = cajones (regla a)
    cs          = celdas a
    izquierdas  = last cs : init cs
    derechas    = tail cs ++ [head cs]
    nuevas      = fmap (nuevoValor tabla) (zip3 izquierdas cs derechas)

cellularInicial :: Int -> [Bool]
cellularInicial n  
  | even n = False : c1
  | otherwise = c1
  where
    m = div (n-1) 2
    c = replicate m False
    c1 = c ++ [True] ++ c  


inicial :: Int -> Int -> Automata
inicial r ancho = Automata { celdas = cellularInicial ancho, regla = r, generacion = 0 }


simbolo :: Bool -> Char
simbolo a  
   | a == True = '#'
   | otherwise = ' '

dibujar :: Automata -> String
dibujar (Automata cel _ _)= fmap simbolo cel -- map/fmap de simbolo sobre las celdas de a
