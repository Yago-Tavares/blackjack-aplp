import System.Random
import Control.Monad (replicateM)
import System.IO.Unsafe
 
baralho = zip ["As de copas", "As de espadas", "As de ouro", "As de paus", "Dois de copas","Dois de espadas", "Dois de ouro", "Dois de paus", "Tres de copas", "Tres de espadas", "Tres de ouro", "Tres de paus", "Quatro de copas", "Quatro de espadas", "Quatro de ouro", "Quatro de paus", "Cinco de copas", "Cinco de espadas", "Cinco de ouro", "Cinco de paus", "Seis de copas", "Seis de espadas", "Seis de ouro", "Seis de paus", "Sete de copas", "Sete de espadas", "Sete de ouro", "Sete de paus", "Oito de copas", "Oito de espadas", "Oito de ouro", "Oito de paus", "Nove de copas", "Nove de espadas", "Nove de ouro", "Nove de paus", "Dez de copas", "Dez de espadas", "Dez de ouro", "Dez de paus", "Valete de copas", "Valete de espadas", "Valete de ouro", "Valete de paus", "Dama de copas", "Dama de espadas", "Dama de ouro", "Dama de paus", "Rei de copas", "Rei de espadas", "Rei de ouro", "Rei de paus"] [1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 8, 9, 9, 9, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10]

tamanhoBaralho = length baralho

baralhoHumano = zip [][]

baralhoMaquina = zip [][]

main = do  
    putStrLn "Bem vindo ao jogo 21!!! Digite seu nome:"  
    name <- getLine  
    putStrLn ("Iaew " ++ name ++ ", cê é zika")  

{- Comentário  em Haskell

baralho !! 0 (pega a posição 0 da tuplha baralho) ["As de copas, 1]

Acessando os valores das tuplas

fst (baralho !! 0) - vai retornar o "As de copas" -
snd (baralho !! 0) - vai retornar o 1 -

valorRandom = unsafePerformIO (getStdRandom (randomR (0, tamanhoBaralho))) - Gera um valor aleatório no range 1~51 de inteiros

Assim podemos pegar uma carta aleatória no baralho:

baralho !! valorRandom

para adicionar uma carta ao baralhoHumano, fazemos assim:

novoBaralho = ([baralhoHumano] ++ [baralho!!valorRandom])

depois, atribui o novoBaralho ao baralhoHumano

baralhoHumano = novoBaralho

-}


