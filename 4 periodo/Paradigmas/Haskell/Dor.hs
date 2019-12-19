import Data.Char

vendas 0 = 12
vendas 1 = 12
vendas 2 = 12
vendas 3 = 5

fakebool :: Int -> Int -> Int
fakebool a b
 |a == b = 1
 |otherwise = 0

percorre :: Int -> Int -> Int
percorre s 0 = fakebool s (vendas 0)
percorre s cont
 |vendas cont == s = 1 + percorre s (cont-1)
 |otherwise = percorre s (cont-1)

vendassemanas :: Int -> Int -> Int
vendassemanas s n = percorre s n

media :: Int -> Int -> Int -> Float
media a b c = fromIntegral(a + b + c)/3

criaLista :: [Int] -> Int -> [Int]
criaLista l n = n:l

--Numero 1, Numero 2, Numero 3 e quantos estao acima da media
acima :: Int -> Int -> Int -> Int
acima a b c = length(filter (\x -> fromIntegral(x) > (media a b c)) (criaLista [] a ++ criaLista [] b ++ criaLista [] c))

addEspacos :: Int -> String
addEspacos 0 = ""
addEspacos n = " " ++ addEspacos (n-1)

paraDireita :: Int -> String -> String
paraDireita n s = addEspacos n ++ s

cabecalho :: String
cabecalho = "Semana         Venda"

totalvendas :: Int -> Int
totalvendas 0 = vendas 0
totalvendas n = vendas n + totalvendas (n-1)

--Valor n de semanas, contador auxiliar que começa de zero, saida
traspraFrente :: Int -> Int -> String
traspraFrente n cont
 |cont == n = (show(cont) ++ "             " ++ show(vendas cont))
 |cont < n = (show(cont) ++ "             " ++ show(vendas cont) ++ "\n") ++ (traspraFrente n (cont+1))

mediaVendas :: Int -> Float
mediaVendas n = fromIntegral(totalvendas n)/fromIntegral(n+1)

juntaString :: Int -> String
juntaString n = "\n" ++ cabecalho ++ "\n" ++ traspraFrente n 0 ++ "\n" ++ "Total: " ++ show(totalvendas n) ++ "\n" ++ "Media: " ++ show(mediaVendas n) ++ "\n"

imprimeTabela :: Int -> IO()
imprimeTabela n = putStr(juntaString n)

maior :: Int -> Int -> Int -> Int
maior x y z
 |x >= y && x >= z = x
 |y >= x && y >= z = y
 |z >= x && z >= y = z

menor :: Int -> Int -> Int -> Int
menor x y z
 |x <= y && x <= z = x
 |y <= x && y <= z = y
 |z <= x && z <= y = z

meio :: Int -> Int -> Int -> Int
meio x y z
 |(x >= y && x <= z) || (x <= y && x >= z) = x
 |(y >= x && y <= z) || (y <= x && y >= z) = y
 |(z >= y && z <= x) || (z <= y && z >= x) = z

menorMaior :: Int -> Int -> Int -> (Int,Int)
menorMaior a b c = (menor a b c, maior a b c)

ordenaTripla :: (Int, Int, Int) -> (Int, Int, Int)
ordenaTripla (a,b,c) = (menor a b c, meio a b c, maior a b c)

type Ponto = (Float, Float)
type Reta = (Ponto, Ponto)

fstcoordenada :: Ponto -> Float
fstcoordenada (a,b) = a

sndcoordenada :: Ponto -> Float
sndcoordenada (a,b) = b

vertical :: Reta -> Bool
vertical (x,y) = x == y

quadrado :: Int -> Int
quadrado n = n*n

mapping :: [Int] -> [Int]
mapping lista = map quadrado lista

somadosquadrados :: [Int] -> Int
somadosquadrados lista = foldr1 (+) (mapping lista)

maiorquezero :: [Int] -> [Int]
maiorquezero lista = filter (\x -> x > 0) lista

--N limitante, contador e lista
intervalo :: Int -> Int -> [Int]
intervalo n cont
 |n == cont = [n]
 |otherwise = cont:(intervalo n (cont+1))

--número de divisores de n no intervalo 1..n
numFactors :: Int -> Int
numFactors n = length(filter (\x -> n `mod` x == 0) (intervalo n 1))

-- lista de inteiros, um inteiro n e quantos da lista são maiores que n
retornaSup :: Int -> [Int] -> Int
retornaSup n lista = length(filter(\x -> x > n) lista)

uniao :: [Int] -> [Int] -> [Int]
uniao [] lista2 = lista2
uniao (x:xs) lista2
 |elem x lista2 = uniao xs lista2
 |otherwise = x:(uniao xs lista2)

mediaMaiores :: Int -> [Int] -> Float
mediaMaiores n lista = fromIntegral(foldr1 (+) filtro)/fromIntegral(length filtro)
 where filtro = (filter(\x -> x > n) lista)

intercala :: [Int] -> [Int] -> [Int]
intercala [] lista2 = lista2
intercala lista1 [] = lista1
intercala (x:xs) (y:ys) = x:y:(intercala xs ys)

ranking :: [String] -> [Int]
ranking [] = []
ranking (x:xs) = (foldr1 (+) (map ord (filter (\z -> isAlpha(z)) x))):(ranking xs)

-- Prova (questão 2)
maisRepetida :: [(String,Int,Int)] -> [(String,Int,Int)]
maisRepetida ((s,i,d):xs) = filter (\(s,i,d) -> d /= 2019) ((s,i,d):xs)

--Caso de teste
--maisRepetida [("Pagode",3,2019),("Samba",34,2019),("Pagode",13,2019),("Funk",53,2019),("Rock",3,2018),("T-rex",3,2019),("Pagode",53,2019),("Rock",32,2018),("Funk",37,2019)]


--Revisando com ponto extra 1

-- Primeira

somaPares :: [Int] -> [Int] -> [Int]
somaPares [] lista2 = lista2
somaPares lista1 [] = lista1
somaPares (x:xs) (y:ys) = (x+y):(somaPares xs ys)

-- Segunda

aux :: String -> String -> String -> Bool
aux [] listax [] = True
aux [] listax lista = True
aux sub listax [] = False
aux (x:xs) listax (y:ys)
 | x == y = aux xs listax ys
 | otherwise = aux listax listax ys

--palavra sublista, palavra lista, resposta
subLista :: String -> String -> Bool
subLista [] lista = True
subLista sub lista = aux sub sub lista

-- Terceira
toLista :: Int -> [Int]
toLista n = [n]

ascendente :: [Int] -> Bool
ascendente [] = True
ascendente [n] = True
ascendente (x:y:xs) = x < y && ascendente(y:xs)

loopLista :: Int -> Int -> [Int]
loopLista n cont
 |n == cont = [cont]
 |cont < n = [cont] ++ loopLista n (cont+1)

crescente :: (Int -> Int) -> Int -> Bool
crescente f 0 = False
crescente f n = ascendente (map f (loopLista n 0))

--Funções de teste auxiliares (não fazem parte da resposta)

f :: Int -> Int
f n = n*2

g :: Int -> Int
g n = 1-n

igual :: Int -> Int
igual n = 5

--Revisando com ponto extra 2

remove :: String -> Char -> String
remove list c = filter (\x -> x /= c) list

pares :: [Int] -> Bool
pares lista = length(filter(\x -> x `mod` 2 == 1) lista) == 0

raizdemaior :: [Float] -> [Float]
raizdemaior lista = map sqrt (filter (\x -> x > 0) lista)

--Faça uma função que dada uma lista de números inteiros e 
--um número inteiro k retorna a multiplicação do cubo dos números que são múltiplos de k.

cubo :: Int -> Int
cubo n = n^3

multi :: [Int] -> Int -> Int
multi lista k = foldr1 (*) (map cubo (filter(\x -> x `mod` k == 0) lista))

forca :: String -> Int
forca s
 | s == "soldado" = 2
 | s == "cavaleiro" = 10
 | s == "catapulta" = 30
 | s == "dragao" = 100
 | otherwise = 0

calculaForca :: [(String, Int)] -> Int
calculaForca [] = 0
calculaForca ((a,b):xs) = (b * forca a) + calculaForca xs

auxforca :: (String, Int) -> Int
auxforca (a,b)
 | a == "soldado" = 2*b
 | a == "cavaleiro" = 10*b
 | a == "catapulta" = 30*b
 | a == "dragao" = 100*b
 | otherwise = 0

calculaForca2 :: [(String, Int)] -> Int
calculaForca2 lista = foldr1 (+) (map auxforca lista)

mytake :: [Int] -> Int -> [Int]
mytake xs 0 = []
mytake [] n = []
mytake (x:xs) n = x:(mytake xs (n-1))

mydrop :: [Int] -> Int -> [Int]
mydrop xs 0 = xs
mydrop [] n = []
mydrop (x:xs) n = mydrop xs (n-1)

alocar :: [Int] -> Int -> [[Int]]
alocar [] n = []
alocar lista n = (take n lista):(alocar (drop n lista) n)