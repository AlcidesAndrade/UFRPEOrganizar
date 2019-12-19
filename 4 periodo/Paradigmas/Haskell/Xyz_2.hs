vendas 0 = 10
vendas 1 = 3
vendas 2 = 7
vendas 3 = 8

addEspacos :: Int -> String
addEspacos 0 = ""
addEspacos 1 = " "
addEspacos n = addEspacos(n-1) ++ " "

paraDireita :: Int -> String -> String
paraDireita 0 x = x
paraDireita 1 x = " " ++ x
paraDireita n x = addEspacos(n) ++ x

cabecalho :: String
cabecalho = "\nSemana        Venda\n"

imprimeSemanas :: Int -> String
imprimeSemanas 0 = "0             " ++ show(vendas 0) ++ "\n"
imprimeSemanas n = imprimeSemanas(n-1) ++ show(n) ++ "             " ++ show(vendas n) ++ "\n"

somaTudo :: Int -> Int
somaTudo 0 = vendas 0
somaTudo n = vendas n + somaTudo(n-1)

imprimeTotal :: Int -> String
imprimeTotal n = "Total: " ++  show(somaTudo(n)) ++ "\n"

calcularMedia :: Int -> Float
calcularMedia n = fromIntegral(somaTudo(n))/fromIntegral(n)

imprimeMedia :: Int -> String
imprimeMedia n = "Media: " ++ show(calcularMedia(n)) ++ "\n"

juntaString :: Int -> String
juntaString n = cabecalho ++ imprimeSemanas n ++ imprimeTotal n ++ imprimeMedia n

imprimeTabela :: Int -> IO()
imprimeTabela n = putStr(juntaString n)

menorMaior :: Int -> Int -> Int -> (Int, Int) --meow meow kill me now
menorMaior a b c
|a > b && b > c = (c,a)
|a > b && b < c = (b,a)
|a < b && b < c = (a,c)
|a < b && 
|a > c && 
|a > c && 
|a < c && 
|a < c && 