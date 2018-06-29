import System.Random as Random
import Control.Monad (replicateM)
import System.IO.Unsafe
import Control.Monad(when) 

baralho = zip ["As de copas", "As de espadas", "As de ouro", "As de paus", "Dois de copas","Dois de espadas", "Dois de ouro", "Dois de paus", "Tres de copas", "Tres de espadas", "Tres de ouro", "Tres de paus", "Quatro de copas", "Quatro de espadas", "Quatro de ouro", "Quatro de paus", "Cinco de copas", "Cinco de espadas", "Cinco de ouro", "Cinco de paus", "Seis de copas", "Seis de espadas", "Seis de ouro", "Seis de paus", "Sete de copas", "Sete de espadas", "Sete de ouro", "Sete de paus", "Oito de copas", "Oito de espadas", "Oito de ouro", "Oito de paus", "Nove de copas", "Nove de espadas", "Nove de ouro", "Nove de paus", "Dez de copas", "Dez de espadas", "Dez de ouro", "Dez de paus", "Valete de copas", "Valete de espadas", "Valete de ouro", "Valete de paus", "Dama de copas", "Dama de espadas", "Dama de ouro", "Dama de paus", "Rei de copas", "Rei de espadas", "Rei de ouro", "Rei de paus"] [1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 8, 9, 9, 9, 9, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10]

baralhoHumano = []

baralhoMaquina = []

--Main da aplicação
main :: IO ()
main = do
 escolherDificuldade

 opcaoInput <- getLine
 let opcao = read opcaoInput :: Int
 	
 if(opcao == 1) then putStrLn "Dificuldade escolhida é: Fácil"
  else if (opcao == 2) then putStrLn "Dificuldade escolhida é: Médio"
   else putStrLn "Dificuldade escolhida é: Difícil"
--linha 30
 iniciarPartida

--Escolher dificuldade do jogo
escolherDificuldade :: IO()
escolherDificuldade = do
 putStrLn "Bem vindo ao jogo 21!"
 putStrLn "Escolha a dificuldade da partida:"
 putStrLn "1 - Fácil"
 putStrLn "2 - Médio"
 putStrLn "3 - Difícil"

--Iniciar partida puxando as cartas
iniciarPartida :: IO()
iniciarPartida = do

-- Pega cartas do humano
   
   --pega primeira carta
   indice1 <- randomRIO(0 :: Int, 51 :: Int)
   let carta1 = baralho !! indice1
   let pontuacaoHumano1 = snd (carta1)

   --remove a carta do baralho
   let primeiraParte = take indice1 baralho
   let proximaCarta = indice1 + 1
   let segundaParte = drop proximaCarta baralho
   let novoBaralho = primeiraParte ++ segundaParte
   
   --pega segunda carta
   indice2 <- randomRIO(0 :: Int, 50 :: Int)
   let carta2 = novoBaralho !! indice2
   let pontuacaoHumano2 = pontuacaoHumano1 + snd (carta2)
   
   --remove a carta do baralho
   let primeiraParte = take indice2 novoBaralho
   let proximaCarta = indice2 + 1
   let segundaParte = drop proximaCarta novoBaralho
   let novoBaralho = primeiraParte ++ segundaParte

   let cartas = [carta1, carta2]
   let novoBaralhoHumano = baralhoHumano ++ cartas
   putStrLn "Baralho Humano: "
   putStrLn((show novoBaralhoHumano) ++ " Pontuação: " ++ (show pontuacaoHumano2))
  

   
------------------------------------------------------
-- Pega cartas da maquina

   --pega primeira carta
   indice1 <- randomRIO(0 :: Int, 48 :: Int)
   let carta1 = novoBaralho !! indice1
   let pontuacaoMaquina1 = snd (carta1)
   
   --remove a carta do baralho
   let primeiraParte = take indice1 novoBaralho
   let proximaCarta = indice1 + 1
   let segundaParte = drop proximaCarta novoBaralho
   let novoBaralho = primeiraParte ++ segundaParte
   
   --pega segunda carta
   indice2 <- randomRIO(0 :: Int, 47 :: Int)
   let carta2 = novoBaralho !! indice2
   let pontuacaoMaquina2 = pontuacaoMaquina1 + snd (carta2)
  
   --remove a carta do baralho
   let primeiraParte = take indice2 novoBaralho
   let proximaCarta = indice2 + 1
   let segundaParte = drop proximaCarta novoBaralho
   let novoBaralho = primeiraParte ++ segundaParte   
   
   let cartas = [carta1, carta2]
   let novoBaralhoMaquina = baralhoMaquina ++ cartas
   
   putStrLn "Baralho Maquina: "
   putStrLn((show novoBaralhoMaquina) ++ " Pontuação: " ++ (show pontuacaoMaquina2))

   --Escolher se puxa nova carta
   putStrLn("Quer puxar outra carta? 1 - sim | 2 - não")
   escolhaTemp <- getLine
   let escolha = read escolhaTemp :: Int
   puxarCarta escolha 48 pontuacaoHumano2 pontuacaoMaquina2 novoBaralhoHumano novoBaralhoMaquina novoBaralho
   
   
-- Exibe o vencedor baseado nos pontos feitos pelos jogadores   
vencedorPartida pontuacaoHumano pontuacaoMaquina = do
	putStrLn "\n----------- Resultado ----------------"
	if ((pontuacaoHumano > pontuacaoMaquina) && (pontuacaoHumano <= 21)) then putStrLn ("Jogador humano é o vencedor: " ++ (show pontuacaoHumano) ++ " pontos" ++ "\nPontuação máquina: " ++ show(pontuacaoMaquina))
	else if ((pontuacaoMaquina > pontuacaoHumano) && (pontuacaoMaquina <= 21)) then putStrLn ("Jogador maquina é o vencedor: " ++ (show pontuacaoMaquina) ++ " pontos" ++ "\nPontuação humano: " ++ show(pontuacaoHumano))
	else if (pontuacaoHumano == pontuacaoMaquina) then putStrLn ("Deu empate\n Jogador humano: " ++ (show pontuacaoHumano) ++ "\n Jogador maquina: " ++ show(pontuacaoMaquina))
	else if (pontuacaoHumano > 21 && pontuacaoMaquina <= 21) then putStrLn ("Jogador maquina é o vencedor: " ++ (show pontuacaoMaquina) ++ " pontos" ++ "\nPontuação humano: " ++ show(pontuacaoHumano))
	else if (pontuacaoMaquina > 21 && pontuacaoHumano <= 21) then putStrLn ("Jogador humano é o vencedor: " ++ (show pontuacaoHumano) ++ " pontos" ++ "\nPontuação máquina: " ++ show(pontuacaoMaquina))
	--Eu acho que aqui podemos reinvocar o inicarPartida e rodar um novo jogo quando der empate de pontos
	else putStrLn ("Não houve ganhadores, ambos jogadores marcaram acima de 21 pontos! \nPontuação humano: " ++ (show pontuacaoHumano) ++ "\nPontuação máquina: " ++ (show pontuacaoMaquina))

-- Parâmetros: Escolha - TamanhoBaralho - PontuaçãoJogadorHumano - PontuaçãoJogadorMáquina - BaralhoJogadorHumano - BaralhoJogadorMáquina - Baralho
puxarCarta :: Int -> Int -> Int -> Int -> [(String, Int)] -> [(String, Int)]-> [(String, Int)] -> IO()
puxarCarta 2 tamanhoBaralho pontuacaoJogadorHumano pontuacaoJogadorMaquina baralhoJogadorHumano baralhoJogadorMaquina baralho = do
  --Isso aqui não tá fazendo nada a não ser imprimindo o resultado do baralho do jogador
  vencedorPartida pontuacaoJogadorHumano pontuacaoJogadorMaquina
  --putStrLn(show(baralhoJogadorHumano) ++ " Pontuação:" ++ (show pontuacaoJogadorHumano))

puxarCarta escolha tamanhoBaralho pontuacaoJogadorHumano pontuacaoJogadorMaquina baralhoJogadorHumano baralhoJogadorMaquina baralho = do

  novoRandom <- randomRIO(0 :: Int, tamanhoBaralho :: Int)
  --Pegando nova carta
  let novaCarta = baralho !! novoRandom
  let novaPontuacaoJogadorHumano = pontuacaoJogadorHumano + snd (novaCarta)
  let novoBaralhoJogadorHumano = baralhoJogadorHumano ++ [novaCarta]
  
  --Removendo a carta do baralho
  let primeiraParte = take novoRandom baralho
  let proximaCarta = novoRandom + 1
  let segundaParte = drop proximaCarta baralho
  let novoBaralho = primeiraParte ++ segundaParte
  
  putStrLn("Baralho: \n" ++ (show novoBaralhoJogadorHumano) ++ " Pontuação: " ++ (show novaPontuacaoJogadorHumano))
  
  print novoBaralho
  
  putStrLn("Quer puxar outra carta? 1 - sim | 2 - não")

--tá faltando remover a carta!
  escolhaTemp <- getLine
  let escolha = read escolhaTemp :: Int
  
   -- esse índice aqui tem de atualizar também 
  let novoTamanhoBaralho = (tamanhoBaralho - 1)

  puxarCarta escolha novoTamanhoBaralho novaPontuacaoJogadorHumano pontuacaoJogadorMaquina novoBaralhoJogadorHumano baralhoJogadorMaquina novoBaralho

--Não tem serventia por enquanto
podeJogar :: Int -> Int
podeJogar pontuacaoJogador = do
	if pontuacaoJogador <= 21 then 1
	else 0
