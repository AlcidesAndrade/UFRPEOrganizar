--(String,Int, Timestamp)
-- Nome, duracao(s), data/hora
--ano(Timestamp) retorna um Int que é o ano
-- As funções usarão Int no lugar de Timestamp

lista = [("Aa",30,2019),("Aa",60,2018),("Aa",120, 2019),("Aa",40, 2019),("Bb",200,2019),("Cc",400,2019),("Dd", 2, 2018),("Ee",35,2019)]

--z == n na questão é ano(z) == n
--filtraAno :: [(String, Int, Int)] -> Int -> [(String,Int,Int)]
-- Retorna uma lista com os valores cujo ano são passados como parâmetro
filtraAno [] _ = []
filtraAno ((a,b,c):xs) n = filter(\(x,y,z) -> z == n) ((a,b,c):xs)

-- Recebe o nome da música, valor 0 e a lista, incrementa o contador caso 
-- encontre mais ocorrências da música e devolve
-- uma tupla (Nome, ocorrencias)
--occMus :: String -> Int -> [(String, Int, Int)] -> (String, Int)
occMus s n [] = (s,n)
occMus st num ((a,b,c):xs)
	| st == a = occMus st (num+1) xs
	| otherwise = occMus st num xs

-- Recebe a lista e retorna uma lista de tuplas com cada musica e suas ocorrencias 
--geralistaoccMus :: [(String, Int, Int)] [(String,Int)] -> [(String, Int)]
geralistaoccMus [] _ = []
geralistaoccMus ((a,b,c):xs) li
	| (elem a li) = geralistaoccMus xs li
	| otherwise = occMus a 0 ((a,b,c):xs) : geralistaoccMus xs (a:li) 


--Dada uma lista de (Nome, ocorrencias), retorna o Nome cuja ocorrencia é maior
--contaRep :: [(String,Int)] "" 0 -> String
contaRep [] s n = s 
contaRep ((a,b):xs) st num
	| b > num = contaRep xs a b
	| otherwise = contaRep xs st num

--input contaRep (geralistaoccMus(filtraAno lista 2019) []) "" 0