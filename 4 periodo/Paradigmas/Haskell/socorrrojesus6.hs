{-crescenteDanielporra f n = checa f (f 0) [1..n]


checa f n [] = True
checa f n (x:xs)
 |f(n) >= f(x) = False
 |otherwise = checa f (x:xs)-}

--Map transforma uma lista em outra lista. A lista retornada tem o mesmo tamanho da outra lista.
--Fold transforma uma lista em um único elemento
--Filter transforma uma lista filtrando ela. A lista pode diminuir ou não.

type Livro = String
type Pessoa = String
type BancoDados = [(Pessoa,Livro)]

sqr :: Int -> Int
sqr n = n * n

quadrado :: [Int] -> [Int]
quadrado l = map sqr l

quadrado2 :: [Int] -> [Int]
quadrado2 l = map (\n -> n*n) l

somatudo :: [Int] -> Int
somatudo l = foldr1 (+) (quadrado l)

maiorque :: [Int] -> [Int]
maiorque l = filter (\n -> n > 0) l

maiores :: [[Int]] -> [Int]
maiores [] = []
maiores l = map maximum l

baseExemplo :: BancoDados
baseExemplo = [("oi","kelvin"),("td","bom"),("com","vc"),("hein","menino?"),("oi","kelvin"),("oi","socorro")]

membro :: [Int] -> Int -> Bool
membro l n = length([a | a <- l, a == n]) /= 0

livros :: BancoDados -> Pessoa -> [Livro]
livros bd p = map snd (filter (\(x,y) -> x == p) bd)