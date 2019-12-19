{-(Fracasso em forma de questão) 1:

data NothingFake = Nothing

data SisArq = Arq String | Dir([SisArq]) | NothingFake

{-Esse tipo representará a configuração dos arquivos. Sendo assim,
nessa questão ele será recebido e andaremos dentro dele para retornar a
string formada com seus arquivos-}

percorredor :: String -> SisArq -> [SisArq]
percorredor arqname NothingFake = NothingFake
percorredor arqname (Dir (x:xs)) = filter(\x -> elem x (x:xs) (x:xs))
-}

