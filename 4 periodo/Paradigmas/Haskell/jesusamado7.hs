{-AQUECIMENTO


1)    Faça uma função que dada uma string e um caractere retorna uma nova string onde todas as ocorrências de do caractere são removidas.

 

2)    Faça uma função que retorna se todos os inteiros de uma lista são pares.

 

3)    Faça uma função que dada uma lista de números, retorna uma lista que contém a raiz quadrada dos elementos que são maiores do que zero.-}

palavra :: [Char] -> Char -> [Char]
palavra s c = filter(\x -> x /= c) s

pares :: [Int] -> Bool
pares l = (length(filter(\x -> x `mod` 2 /= 0) l)) == 0 


paresfold :: [Int] -> Bool
paresfold l = foldr1 (&&) (map even l)

raiz l = map sqrt(filter(\x -> x > 0) l)
{-Começando a atividade em lista-}
{-Faça uma função que dada uma lista de números inteiros e um número inteiro k retorna a multiplicação do cubo dos números que são múltiplos de k.-}

func :: [Int] -> Int -> Int
func l n = foldr1 (*) (map(\y -> y^3) (filter (\x -> x `mod` n == 0) l))

{-Defina uma função que dada uma string representando algum texto, retorna uma lista de pares onde cada par corresponde a uma palavra da lista no primeiro elemento e a quantidade de vezes que ela ocorre no texto no segundo elemento. OBS: vocês devem considerar como separadores de palavras tanto espaços em branco quanto caracteres de nova linha (\n). Exemplo:

 

contaPalavras “o ceu esta azul\nazul da cor do mar”

retorna -> [(“o”,1),(“ceu”,1), (“esta”,1),(“azul”,2),(“da”,1),(“cor”,1), (“do”,1),(“mar”,1)]-}


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

empty :: String -> Bool
empty s = s == [] || s == "\n"

chartostring :: Char -> String
chartostring c = [c]

cortapalavramagica :: String -> String
cortapalavramagica s = 