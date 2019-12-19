import Data.Char

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

{-Defina uma função que recebe uma lista de listas de elementos de um tipo t (genérico) e retorna uma
lista de tuplas-2 onde o primeiro elemento é um valor do tipo t que existe em pelo menos uma das
sub-listas da entrada e o segundo é o número de ocorrências desse valor nas sub-listas.
Exemplos: contaOcorr ["haskell","eh","legal"] retorna
[('h',2),('a',2),('s',1),('k',1),('e',3),('l',4),('g',1)]
contaOcorr [[2,45,6,2,1],[7,7,4,3,2]] retorna
[(2,3),(45,1),(6,1),(1,1),(7,2),(4,1),(3,1)]-}

{-semRepetir :: [T] -> [T] --recebe uma lista e devolve ela sem repetição de elementos
semRepetir [] = []
semRepetir lista = [x | x <- lista, not (elem x lista)]
-- maioresQue lista n = [x | x <- lista, x > n]-}
{-
--cortaVetor :: [[T]] -> [T] --corta a matriz em um vetor de caracteres da primeira linha encontrada
cortaVetor [] =  []
cortaVetor ((x:xs):xz) = x:cortaVetor((xs):xz)

--cortadorMatriz :: [[T]] -> [T] --essa função é responsável por sair manter o loop de corte dos vetores
cortadorMatriz [] = []
cortadorMatriz (x:xs) = cortaVetor(x:xs):cortadorMatriz xs

--contaElemento :: [t] -> t -> Int
contaElemento [] e = 0
contaElemento (x:xs) e
 |x == e = 1 + contaElemento(x:xs) e --se a cabeça é igual ao elemento, encontramos uma ocorrencia, contamos e continuamos olhando a lista
 |otherwise = contaElemento(x:xs) e --não achamos nada igual, então é só continuar olhando a lista

--geraInt :: [T] -> T -> [Int]
geraInt [] e = []
geraInt lista e = [x | x <- contaElemento lista e]

--boraRepetir :: [T] -> [T] -> [Int] --lista toda, lista sem repetição, lista de inteiros
boraRepetir lista [] e = []
boraRepetir tudo (x:xs) = (geraInt(tudo x)):boraRepetir(tudo xs)

--removeElementos :: [T] -> T -> [T]
removeElementos lista t = [x | x <- lista, x /= t]

--seguindo :: [T] -> [T]
seguindo [] = []
seguindo (x:xs) = x:seguindo(removeElementos(x:xs) x)

--contaOcorr :: [[T]] -> [(T,Int)]
contaOcorr [] = []
contaOcorr matriz = (juntei (cortadorMatriz matriz) (boraRepetir(matriz (seguindo matriz))))

--juntei :: [T] -> [Int] -> [T,Int]
juntei [] i = []
juntei lista conta = [(x,y) | x <- lista, y <- conta] -}

somaString :: String -> Int
somaString [] = 0
somaString (x:xs)
 |isAlpha(x) = ord(x)+somaString(xs)
 |otherwise = somaString(xs)

--Opção por compreensão de lista

lerString :: [String] -> [Int]
lerString [] = []
lerString matriz = [somaString x | x <- matriz]

--Opção por concatenação normal
{-lerString :: [String] -> [Int]
lerString [] = []
lerString (x:xs) = somaString(x):lerString(xs)-}

--fatiaTudoUm :: [[T]] -> [T] --recebe uma matriz, devolve a primeira linha
fatiaTudoUm ([]:xz) = []
fatiaTudoUm ((x:xs):xz) = x : fatiaTudoUm((xs):xz)

--fatiaTudoDois :: [[T]] -> [T] --recebe uma matriz e devolve sua versão com todas as linhas em uma unica linha (ela que garante a repetição de fatiaTudoUm para cortar a matriz inteira e juntar numa uma lista)
fatiaTudoDois [] = []
fatiaTudoDois (a:as) = (fatiaTudoUm (a:as)) ++ fatiaTudoDois as

--contaElemento :: [T] -> T -> Int --recebe uma matriz e um elemento a buscar
contaElemento [] t = 0
contaElemento (x:xs) t
 | x == t = 1 + contaElemento(xs) t --se a cabeça da lista for igual ao elemento, contamos e continuamos olhando a lista
 | otherwise = contaElemento(xs) t --senao, so continuamos olhando

--removeElementos :: [[T]] -> T -> [[T]] --recebe uma matriz e um elemento, e devolve a matriz sem o elemento (uma nova matriz sem ele)
removeElementos [] t = []
removeElementos (x:xs) t = filter (/= t) x : removeElementos xs t --filter: (lembre que x é um vetor) forme uma lista com os elementos de x que não são iguais a t e prossiga a concatenação com a cauda (recursão)

--formaTupla :: [T] -> T -> (T, Int) --recebe um vetor (ja sendo a matriz toda juntinha) e um elemento
formaTupla lista t = (t, contaElemento lista t) --pegue a lista, o elemento t e forme uma tupla usando o elemento t no primeiro termo e aplicando no segundo termo a função de contarElemento

--contaOcorr :: [[T]] -> [(T, Int)] --receba uma matriz e devolva sua lista de tuplas
contaOcorr ([]:xz) = contaOcorr(xz)
contaOcorr [] = []
contaOcorr ((x:xs):xz) = (formaTupla (fatiaTudoDois((x:xs):xz)) x) : contaOcorr(removeElementos((x:xs):xz) x)
--sobre a linha anterior: passando a matriz, damos como resposta: formaTupla recebendo a matriz toda junta em uma lista e um elemento x da cabeça (devolvemos esse elemento x como primeiro termo da tupla) e concatenamos com
--uma chamada recursiva da função, recebendo uma nova matriz sem o elemento anterior, pra continuar montando a tupla até chegarmos ao final da matriz

{-
colocaNaLista a l = a:l --uma função auxiliar que coloca um elemento qualquer numa lista

primeiraOcorr l [] = [] --quem preenche a lista sem repetições
primeiraOcorr l (a:as)
 | (length l)==0 = a:primeiraOcorr (colocaNaLista a l) as
 | elem a l = primeiraOcorr l as
 | otherwise = a:primeiraOcorr (colocaNaLista a l) as


auxiliar [] l = [] --quem monta a tupla
auxiliar (a:as) l = (a, length [x|x<-l, x==a]):auxiliar as l

{-
eu entendi o papel de auxiliar, pq ele quem monta a tupla,
recebendo uma lista com apenas os elementos diferentes (sem repetição) 
e uma lista com todos os outros, pra usar a compreensão-}

{-gerando o primeiro termo da tupla usando a lista sem repetições e
usando compreensão pra pegar na lista que tem todos os elementos 
(com repetição), gerando uma lista de um elemento "a" repetido e medir o tamanho dela, 
pra saber o segundo termo da tupla (número de ocorrencias)-}

contaOcorr [] [] [] = [] --quem junta todas as funções para trabalharem juntas	
contaOcorr l1 l2 l3 = auxiliar (primeiraOcorr [] (l1++l2++l3)) (l1++l2++l3)

{-
a função contaOcorr é só uma função que trabalha chamando todo mundo, pegando as listas de entrada, 
chamando a função que monta a tupla (auxiliar) passando como primeiro 
argumento a função que devolve a lista sem repetição e como segundo a lista completa de entrada
-}

-}