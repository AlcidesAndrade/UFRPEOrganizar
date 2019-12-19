{-
putNtimes :: Int -> String -> IO()
putNtimes n str = 
 if n <= 1 then putStr str
 else do 
 putStr str
 putNtimes (n - 1) str

main :: IO()
main = do {
 putStrLn "Escrevendo";
 writeFile "a.txt" "Hello\nworld";
 appendFile "a.txt" "\nof\nHaskell";
 putStrLn "Lendo o arquivo";
 x <- readFile "a.txt";
 putStrLn x;
}

-}

data Fila t = Fila (Int, [t])

push :: t -> Fila t -> Either String (t, Fila t)
push x (Fila(tam, l))
 | length l == tam = Left "Nop"
 | otherwise = Right (x, Fila (tam, l ++ [x]))