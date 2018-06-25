import System.Random
import Control.Monad (replicateM)
import System.IO.Unsafe
import Control.Monad(when) 
 
baralho = zip ["As de copas", "As de espadas", "As de ouro", "As de paus", "Dois de copas","Dois de espadas", "Dois de ouro", "Dois de paus", "Tres de copas", "Tres de espadas", "Tres de ouro", "Tres de paus", "Quatro de copas", "Quatro de espadas", "Quatro de ouro", "Quatro de paus", "Cinco de copas", "Cinco de espadas", "Cinco de ouro", "Cinco de paus", "Seis de copas", "Seis de espadas", "Seis de ouro", "Seis de paus", "Sete de copas", "Sete de espadas", "Sete de ouro", "Sete de paus", "Oito de copas", "Oito de espadas", "Oito de ouro", "Oito de paus", "Nove de copas", "Nove de espadas", "Nove de ouro", "Nove de paus", "Dez de copas", "Dez de espadas", "Dez de ouro", "Dez de paus", "Valete de copas", "Valete de espadas", "Valete de ouro", "Valete de paus", "Dama de copas", "Dama de espadas", "Dama de ouro", "Dama de paus", "Rei de copas", "Rei de espadas", "Rei de ouro", "Rei de paus"] [1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 8, 9, 9, 9, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10]

tamanhoBaralho = length baralho

valor = 0

--valor = take 10 $ randomRs (0,51) (mkStdGen 192391) :: [Int]
--retorna uma lista de 10 valores aleatórios, o problema tá no mkStdGen que vai fazer a lista ser sempre igual

baralhoHumano = zip [][]

baralhoMaquina = zip [][]

{- Este método só aceita como resposta o valor 7 para adicionar 
uma carta no baralho qualquer outro valor ele entra no 'x'
para adicionar uma carta use:

baralhoHumano = adicionarCartaBaralho 7

-}
--adicionarCartaBaralho :: (Integral humano) => humano -> ([Char],Int)   
--adicionarCartaBaralho 7 = baralho!!valor --devia usar valor aqui, mas o tipo é diferente  
--adicionarCartaBaralho x = ("casoDeErroParaTeste",0)  

adicionarCartaBaralho num = baralho !! num


geraValoresAleatorio numero = take 20 $ randomRs (0,51) (mkStdGen numero) :: [Int]
	

	
valorEntrada = do
	putStrLn "Insira um valor inteiro para gerar random:"
        num <- readLn :: IO Int
        return $ num
        
        
{- Comentário  em Haskell

baralho !! 0 (pega a posição 0 da tuplha baralho) ["As de copas", 1]

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


