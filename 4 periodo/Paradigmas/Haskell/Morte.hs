import Data.Char

-- *******Primeiro assunto!*******

fat :: Int -> Int
fat 0 = 0
fat 1 = 1
fat n = n * fat (n-1)

numEqual :: Int -> Int -> Bool
numEqual n m
 |n == m = True
 |otherwise = False

all4Equal :: Int -> Int -> Int -> Int -> Bool
all4Equal a b c d
 |(numEqual a b && numEqual c d && numEqual a c) == True = True
 |otherwise = False

-- loopCompare :: Int -> Int -> Int -> Int -> Int
-- loopCompare a b c aux 
-- |a == b = aux + loopCompare(a b c aux+1)
-- |otherwise = 0

equalCount :: Int -> Int -> Int -> Int
equalCount a b c
 |(a == b) && (a == c) = 3
 |(a == b) || (a == c) || (b == c) = 2
 |otherwise = 0

--Pega 3 numeros e calcula a média
media :: Int -> Int -> Int -> Float
media a b c = fromIntegral(a + b + c)/3

--INCOMPLETO!
{-concatena :: Int -> [Int] -> [Int]
concatena n ls = n:ls

criaLista :: Int -> Int -> Int -> [Int]
criaLista a b c = concatena a [] ++ concatena b [] ++ concatena c []

maiorQue :: Int -> Int -> Bool
maiorQue a b = a > b

--Pega 3 numeros e diz quantos estão acima da media entre eles
--acimaMedia :: Int -> Int -> Int -> Int
--acimaMedia a b c = length(filter(maiorQue(media a b c)) (criaLista a b c))-}

-- *******Segundo assunto*******

-- Funções com caracteres encontradas na biblioteca Data.Char:

-- ord :: Char -> Int
-- chr :: Int -> Char

addEspacos :: Int -> String
addEspacos 0 = ""
addEspacos 1 = " "
addEspacos n = addEspacos (n-1) ++ " "

paraDireita :: Int -> String -> String
paraDireita 0 s = s
paraDireita n [] = []
paraDireita n s = addEspacos n ++ s 

vendas 0 = 15
vendas 1 = 10
vendas 2 = 3
vendas 3 = 2
vendas 4 = 3

maxvendas :: Int -> Int
maxvendas 0 = 0
maxvendas n = vendas n + maxvendas n-1

--Numero a comparar -> Contador -> Ocorrencias
percorreSemanas :: Int -> Int -> Int
percorreSemanas n 0 = 0
percorreSemanas n cont
 |vendas cont == n = 1 + percorreSemanas n (cont-1) 
 |otherwise = percorreSemanas n (cont-1)

--Recebe um s qualquer, um numero de semanas e retorna quantas semanas de 0 a n tiveram vendas igual a s
semanas :: Int -> Int -> Int
semanas s 0 = 0
semanas s n = percorreSemanas s n

cabecalho :: String
cabecalho = "\nSemana   Venda\n"

imprimeSemanas :: Int -> String
imprimeSemanas 0 = "0        " ++ show(vendas 0) ++ "\n"
imprimeSemanas n = imprimeSemanas (n-1) ++ show(n) ++ "        " ++ show(vendas n) ++ "\n"

total :: Int -> Int
total 0 = vendas 0
total n = total (n-1) + vendas n

minhaMedia :: Int -> Float
minhaMedia n = fromIntegral(total n)/3

juntaString :: Int -> String
juntaString n = cabecalho ++ imprimeSemanas n ++ "Total: " ++ show(total n) ++ "\n" ++ "Media: " ++ show(minhaMedia n) ++ "\n"

imprimeTabela :: Int -> IO()
imprimeTabela n = putStr(juntaString n)


-- ******Trabalhando com tuplas******


maior :: Int -> Int -> Int -> Int
maior a b c
 |(a >= b) && (a >= c) = a
 |(b >= a) && (b >= c) = b
 |(c >= a) && (c >= b) = c

menor :: Int -> Int -> Int -> Int
menor a b c
  |(a <= b) && (a <= c) = a
  |(b <= a) && (b <= c) = b
  |(c <= a) && (c <= b) = c

meio :: Int -> Int -> Int -> Int
meio a b c
 |maior a b c == a && menor a b c == b = c
 |maior a b c == b && menor a b c == c = a
 |maior a b c == c && menor a b c == a = b
 |menor a b c == a && maior a b c == b = c
 |menor a b c == b && maior a b c == c = a
 |menor a b c == c && maior a b c == a = b

menorMaior :: Int -> Int -> Int -> (Int, Int)
menorMaior a b c = (menor a b c, maior a b c)

ordenaTripla :: (Int, Int, Int) -> (Int, Int, Int)
ordenaTripla (a,b,c) = (menor a b c, meio a b c, maior a b c)

type Ponto = (Float, Float)
type Reta = (Ponto, Ponto)

fstCoord :: Ponto -> Float
fstCoord p = fst p

-- Opção 2 (sem funções auxiliares):
-- fstCoord :: Ponto -> Float
-- fstCoord (a,b) = a

sndCoord :: Ponto -> Float
sndCoord p = snd p

-- Opção 2 (sem funções auxiliares):
-- sndCoord :: Ponto -> Float
-- sndCoord (a,b) = b

vertical :: Reta -> Bool
vertical r = (fst r == snd r)

-- Opção 2 (sem funções auxiliares):
-- vertical :: Reta -> Bool
-- vertical (a,b) = a == b

-- ******Dia 3 e Dia 4******
-- Alta Ordem e Listas

funcao :: Int -> Int
funcao n = n * 2

--Função, Limite, Contador auxiliar
percorre :: (Int -> Int) -> Int -> Int -> Bool
percorre f n i
 |i == n = True
 |f i < f(i+1) = percorre f n (i+1)
 |otherwise = False

diminui :: Int -> Int
diminui n = n - n*2

crescente :: (Int -> Int) -> Int -> Bool
crescente f 0 = True
crescente f n = percorre f n 0

aoquadrado :: Int -> Int
aoquadrado n = n*n

quadrado :: [Int] -> [Int]
quadrado lista = map aoquadrado lista

somadosquadrados :: [Int] -> Int
somadosquadrados lista = foldr1 (+) (map aoquadrado lista)

maiorque0 :: [Int] -> [Int]
maiorque0 lista = filter(\x -> x > 0) lista

maximo :: [Int] -> Int
maximo [] = 0
maximo (x:xs)
 |x >= maximo xs = x
 |otherwise = maximo xs

maiores :: [[Int]] -> [Int]
maiores lista = map maximo lista

-- ****Aula prática 1****

criaIntervalo :: Int -> Int -> [Int]
criaIntervalo 0 cont = []
criaIntervalo n cont
 |n == cont = [n]
 |n > cont = cont:criaIntervalo n (cont+1)

divide :: Int -> Int -> Bool
divide n m = n `mod` m == 0

numFactors :: [Int] -> Int -> Int
numFactors lista n = length(filter(\x -> x `mod` n == 0) lista)

retornaSup :: Int -> [Int] -> [Int]
retornaSup n lista = filter(\x -> x > n) lista

elemToList :: Int -> [Int]
elemToList n = [n]

removeList :: [Int] -> [Int]
removeList (x:xs) = xs

uniao :: [Int] -> [Int] -> [Int]
uniao [] lista2 = [] ++ lista2
uniao (x:xs) lista2
 |elem x lista2 = uniao xs lista2
 |otherwise = x:(uniao xs lista2)

mediaMaiores :: Int -> [Int] -> Float
mediaMaiores n lista = fromIntegral((foldr1 (+) (retornaSup n lista)))/fromIntegral((length (retornaSup n lista)))

--Flag auxiliar, lista 1, lista 2
-- auxiliadora :: Int -> [Int] -> [Int] -> [Int]
-- auxiliadora n [] [] = []
-- auxiliadora n (x:xs) (y:ys) 
--  |n `mod` 2 == 0 = x:(auxiliadora (n+1) xs (y:ys))
--  |otherwise = y:(auxiliadora (n+1) (x:xs) ys)
  
-- intercala :: [Int] -> [Int] -> [Int]
-- intercala lista1 lista2 = auxiliadora 0 lista1 lista2

intercala :: [Int] -> [Int] -> [Int]
intercala [] [] = []
intercala (x:xs) (y:ys) = x : y : (intercala xs ys)

somaFrase :: String -> Int
somaFrase frase = foldr1 (+) (map ord (filter (\x -> isAlpha x) frase))

ranking :: [String] -> [Int]
ranking [] = []
ranking (x:xs) = (somaFrase x):(ranking xs)

--cortaLista :: [[T]] -> [T]
cortaLista ([]:xs) = []
cortaLista (x:xs) = x

--juntaMatriz :: [[T]] -> [T]
juntaMatriz [] = []
juntaMatriz (x:xs) = (cortaLista (x:xs)) ++ juntaMatriz xs

--contaElemento :: [T] -> T -> Int
contaElemento [] t = 0
contaElemento (x:xs) t
 |t == x = 1 + contaElemento (xs) t
 |otherwise = contaElemento (xs) t

--removeElementos :: [[T]] -> T -> [[T]]
removeElementos [] t = []
removeElementos (x:xs) t = filter (/= t) x : removeElementos(xs) t

--formaTupla :: [T] -> T -> (T, Int)
formaTupla lista t = (t, contaElemento lista t)

--contaOcorr :: [[T]] -> [(T,Int)]
contaOcorr ([]:xz) = contaOcorr(xz)
contaOcorr [] = []
contaOcorr ((x:xs):xz) = (formaTupla (juntaMatriz ((x:xs):xz)) x) : (contaOcorr (removeElementos ((x:xs):xz) x))

-- *******Prova 1VA*******

--Questão 3

-- data PROP = Nome String | And (PROP) (PROP) | Or (PROP) (PROP) | Not (PROP)

-- avalia :: PROP -> [(String, Bool)] -> Bool
-- avalia p [] = False
-- avalia And P Q l = (avalia P l) && (avalia Q l) 
-- avalia Or P Q l = (avalia P l) || (avalia Q l)
-- avalia Not P = not (avalia P l)
-- avalia Nome s = nomeValor (s l)

-- nomeValor :: String -> [(String, Bool)] -> Bool
-- nomeValor s [] = False
-- nomeValor s (x:xs)
--  |s == fst(x) = snd(x)
--  |otherwise = nomeValor s xs

--Questão 5

-- data INCIDENTE = Roubo Float | Homicidio Int | Agressao Int

-- crime :: String -> Int
-- crime arq = do{
--   x <- readFile arq;
--   inc <- processaArquivo x;
--   return crimeAux inc;
-- }

-- crimeAux :: [Maybe INCIDENTE] -> Int
-- crimeAux (Nothing: xs) = 0 + crimeAux xs
-- crimeAux (Just x:xs) = processaAux x + crimeAux xs

-- processaAux :: INCIDENTE -> Int
-- processaAux Roubo x
--  | x <= 1000.50 = 5
--  | otherwise = 10
-- processaAux Homicidio n = n * 20
-- processaAux Agressao n = n * 5


-- -- servidores :: [(String,Float)] -> [String]
-- -- servidores [] = []
-- -- servidores lista = map fst (filter (\(x,y) -> y >(fromIntegral(foldr1 (+) (map snd lista))/(fromIntegral(length lista))) lista)

-- 3 questão 1VA

-- data PROP = Var String | AND (PROP) (PROP) | OR (PROP) (PROP) | NOT (PROP) 

-- avalia :: PROP -> [(String, Bool)] -> Bool
-- avalia p [] = False
-- avalia AND P Q lista = (avalia P lista) && (avalia Q lista)
-- avalia OR P Q lista = (avalia P lista) || (avalia Q lista)
-- avalia NOT P lista = not (avalia P lista)
-- avalia Var s lista = consulta s lista

-- consulta :: String -> [(String, Bool)] -> Bool
-- consulta s [] = False
-- consulta s (x:xs)
--  | s == fst x = snd x
--  | otherwise = consulta s xs

-- data INCIDENTE = Roubo Float | Homicidio Int | Agressao Int

-- crime :: String -> Int
-- crime arq = do{
--   x <- readFile arq;
--   ini <- processaArquivo x;
--   return processaCrime ini;
-- }

-- processaCrime :: [Maybe INCIDENTE] -> Int
-- processaCrime (Nothing:xs) = 0 + processaCrime xs
-- processaCrime (Just x:xs) = gravidade + processaCrime xs

-- gravidade :: INCIDENTE -> Int
-- gravidade Roubo n
--  | n <= 1000.50 = 5
--  | otherwise = 10
-- gravidade Homicidio n = n * 20
-- gravidade Agressao n = n * 5

-- readFile :: String -> IO String
-- processaArquivo :: IO String -> [Maybe INCIDENTE]