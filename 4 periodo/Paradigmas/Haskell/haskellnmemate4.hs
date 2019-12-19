double :: [Int] -> [Int]
double [] = []
double (x:xs) = (2*x): double xs 

member :: [Int] -> Int -> Bool
member [] a = False
member (x:xs) a
 |x == a = True
 |otherwise = member xs a

digits :: String -> String
digits [] = []
digits(x:xs)
 | x >= '0' && x <= '9' = x:digits xs
 | otherwise = digits xs

sumPairs :: [(Int, Int)] -> [Int]
sumPairs [] = []
sumPairs((x,y):xs) = (x+y):sumPairs xs 

mytake :: [Int] -> Int -> [Int]
mytake xs 0 = []
mytake [] n = []
mytake (x:xs) n = x:(mytake xs (n-1))

mydrop :: [Int] -> Int -> [Int]
mydrop xs 0 = xs
mydrop [] n = []
mydrop (x:xs) n = mydrop xs (n-1)

mytakewhile f (x:xs)
 | f x = x:(mytakewhile f xs)
 | otherwise = mytakewhile f xs
 
mydropwhile f (x:xs)
 |f x = mydropwhile f xs
 |otherwise = x:(mydropwhile f xs)

type Pessoa = String
type Livro = String
type BancoDados = [(Pessoa, Livro)]

membro :: [Int] -> Int -> Bool
membro [] n = False
membro (x:xs) n
 |x == n = True
 |otherwise = membro xs n

baseExemplo :: BancoDados
baseExemplo = [("oi","kelvin"),("td","bom"),("com","vc"),("hein","menino?"),("oi","kelvin"),("oi","kelvin")]

livros :: BancoDados -> Pessoa -> [Livro]
livros [] p = []
livros bd "" = []
livros ((x,y):xs) p -- x,y formam a combinação (pessoa,livro) e xs é a cauda da lista
 |fst(x,y) == p = snd(x,y):(livros xs) p --No banco nós temos essa pessoa? Se sim, mande seu livro
 |otherwise = livros xs p

emprestimos :: BancoDados -> Livro -> [Pessoa]
emprestimos [] book = []
emprestimos bd "" = []
emprestimos ((x,y):xs) book -- x,y formam a combinação (pessoa,livro) e xs é a cauda da lista
 |snd(x,y) == book = fst(x,y):(emprestimos xs) book --No banco nós temos essa pessoa? Se sim, mande seu livro
 |otherwise = emprestimos xs book

emprestado :: BancoDados -> Livro -> Bool
emprestado [] book = False
emprestado bd "" = False
emprestado ((x,y):xs) book
 |snd(x,y) == book = True
 |otherwise = emprestado xs book

qtdlivros :: BancoDados -> Pessoa -> Int
qtdlivros [] p = 0
qtdlivros bd "" = 0
qtdlivros bd p = length(livros bd p)

emprestar :: BancoDados -> Pessoa -> Livro -> BancoDados
emprestar bd p l = bd ++ [(p,l)]

devolver :: BancoDados -> Pessoa -> Livro -> BancoDados
devolver bd pessoa book = [(p,l) | (p,l) <- bd, p /= pessoa && l /= book]

numFactores n = length ([x | x <- [1..n], (mod n x) == 0])

retornaSup :: Int -> [Int] -> Int
retornaSup n [] = 0
retornaSup n lista = length ([x | x <- lista, x > n])

maioresQue :: [Int] -> Int -> [Int]
maioresQue [] n = []
maioresQue lista n = [x | x <- lista, x > n]

uniao :: [Int] -> [Int] -> [Int]
uniao [] n = n
uniao m [] = m
uniao (a:as) b
 |elem a b = uniao as b
 |otherwise = a:(uniao as b)

somaLista :: [Int] -> Int
somaLista [] = 0
somaLista (x:xs) = x+somaLista(xs)

supMedia :: [Int] -> Int -> Float
sup [] n = fromIntegral(0)
sup lista 0 = fromIntegral(0)
supMedia lista n = fromIntegral(somaLista(maioresQue lista n))/fromIntegral(length(maioresQue lista n))

intercala :: [Int] -> [Int] -> [Int]
intercala [] n = n
intercala m [] = m
intercala (x:xs) (y:ys)
 |length(x:xs) == length(y:ys) = x:(intercala xs (y:ys))
 |otherwise = y:(intercala (x:xs) ys)

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Parte 2, o verdadeiro INFERNO

cortaVetor :: [[T]] -> [T]
cortaVetor [] =  []
cortaVetor ((x:xs):xz) = x:cortaJuizo((xs):xz)

cortadorMatriz :: [[T]] -> [T]
cortadorMatriz [] = []
cortadorMatriz (x:xs) = cortaVetor(x:xs):cortadorMatriz xs



contaOcorr :: [[T]] -> [(T,Int)]
