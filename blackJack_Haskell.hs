import System.Random as Random
import Control.Monad (replicateM)
import System.IO.Unsafe
import Control.Monad(when)
import System.Exit (exitSuccess)

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
   
   putStrLn("")
   putStrLn("Sua pontuação: " ++ (show pontuacaoHumano2))
   putStrLn("== Sua Mão ===========")
   imprimir novoBaralhoHumano

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
   
   if (pontuacaoMaquina2 >= 21 || pontuacaoHumano2 >=21)
     then do
       putStrLn $ "== Mao da Máquina ========"
       imprimir novoBaralhoMaquina
     else do
      putStrLn $ "Uma das cartas da Maquina: "
      imprimirMaquina novoBaralhoMaquina 1

   --Escolher se puxa nova carta
   putStrLn("Deseja puxar outra carta? 1 - Sim | 2 - Não")
   escolhaTemp <- getLine
   let escolha = read escolhaTemp :: Int
   puxarCarta escolha 48 pontuacaoHumano2 pontuacaoMaquina2 novoBaralhoHumano novoBaralhoMaquina novoBaralho
   
   
-- Exibe o vencedor baseado nos pontos feitos pelos jogadores   
vencedorPartida pontuacaoHumano pontuacaoMaquina = do

    putStrLn "\n      _.-=- O jogo acabou! -=-._      "

    putStrLn "\n----------- RESULTADO FINAL ----------------"
    if ((pontuacaoHumano > pontuacaoMaquina) && (pontuacaoHumano <= 21)) then putStrLn ("Com " ++ (show pontuacaoHumano) ++ " pontos, o JOGADOR HUMANO venceu!" ++ "\nPontuação da máquina: " ++ show(pontuacaoMaquina))
    else if ((pontuacaoMaquina > pontuacaoHumano) && (pontuacaoMaquina <= 21)) then putStrLn ("Com " ++ (show pontuacaoMaquina) ++ " pontos, a MÁQUINA venceu!" ++ "\nPontuação do jogador humano: " ++ show(pontuacaoHumano))
    else if (pontuacaoHumano == pontuacaoMaquina) then putStrLn ("Os dois jogadores empataram com " ++ (show pontuacaoHumano) ++ " pontos!")
    else if (pontuacaoHumano > 21 && pontuacaoMaquina <= 21) then putStrLn ("Com " ++ (show pontuacaoMaquina) ++ " pontos, a MÁQUINA é a vencedora!" ++ "\nPontuação do jogador humano: " ++ show(pontuacaoHumano))
    else if (pontuacaoMaquina > 21 && pontuacaoHumano <= 21) then putStrLn ("Com " ++ (show pontuacaoHumano) ++ " pontos, o JOGADOR HUMANO é o vencedor!" ++ "\nPontuação da máquina: " ++ show(pontuacaoMaquina))
    
    --Eu acho que aqui podemos reinvocar o inicarPartida e rodar um novo jogo quando der empate de pontos
    else putStrLn ("Ambos os jogadores marcaram acima de 21 pontos! \nPontuação humano: " ++ (show pontuacaoHumano) ++ "\nPontuação máquina: " ++ (show pontuacaoMaquina))

-- Parâmetros: Escolha - TamanhoBaralho - PontuaçãoJogadorHumano - PontuaçãoJogadorMáquina - BaralhoJogadorHumano - BaralhoJogadorMáquina - Baralho
puxarCarta :: Int -> Int -> Int -> Int -> [(String, Int)] -> [(String, Int)]-> [(String, Int)] -> IO()
puxarCarta 2 tamanhoBaralho pontuacaoJogadorHumano pontuacaoJogadorMaquina baralhoJogadorHumano baralhoJogadorMaquina baralho = do
  --Isso aqui não tá fazendo nada a não ser imprimindo o resultado do baralho do jogador
    putStrLn("\n== Sua Mão ===========")
    imprimir baralhoJogadorHumano
    putStrLn $ "\n== Mao da Máquina ========"
    imprimir baralhoJogadorMaquina
    vencedorPartida pontuacaoJogadorHumano pontuacaoJogadorMaquina

  
puxarCarta escolha tamanhoBaralho pontuacaoJogadorHumano pontuacaoJogadorMaquina baralhoJogadorHumano baralhoJogadorMaquina baralho = do
  
  novoRandom <- randomRIO(0 :: Int, tamanhoBaralho :: Int)
  --Pegando nova carta humano
  let novaCarta = baralho !! novoRandom
  let novaPontuacaoJogadorHumano = pontuacaoJogadorHumano + snd (novaCarta)
  let novoBaralhoJogadorHumano = baralhoJogadorHumano ++ [novaCarta]

  --Removendo a carta do baralho (humano)
  let primeiraParte = take novoRandom baralho
  let proximaCarta = novoRandom + 1
  let segundaParte = drop proximaCarta baralho
  let novoBaralho = primeiraParte ++ segundaParte

  decisao <- randomRIO(0 :: Int, 9 :: Int)
  novoRandomMaquina <- randomRIO(0 :: Int, tamanhoBaralho :: Int)
  let novaCartaMaquina = baralho !! novoRandomMaquina
  let novaPontuacaoJogadorMaquina = if (decisao > 4) then (pontuacaoJogadorMaquina + snd (novaCartaMaquina)) else (pontuacaoJogadorMaquina)
  let novoBaralhoJogadorMaquina = if (decisao > 4) then (baralhoJogadorMaquina ++ [novaCartaMaquina]) else (baralhoJogadorMaquina)
  
  --Removendo a carta do baralho (maquina)
  let primeiraParteMaquina = take novoRandomMaquina baralho
  let proximaCartaMaquina = novoRandomMaquina + 1
  let segundaParteMaquina = drop proximaCartaMaquina baralho
  let baralhoFinal = if (decisao > 4) then (primeiraParteMaquina ++ segundaParteMaquina) else (novoBaralho)
  
  putStrLn("")
  putStrLn("Sua pontuação: " ++ (show novaPontuacaoJogadorHumano))
  putStrLn("== Sua Mão ===========")
  imprimir novoBaralhoJogadorHumano
  
  putStrLn("")
  if (novaPontuacaoJogadorMaquina >= 21 || novaPontuacaoJogadorHumano >=21)
     then do
       putStrLn $ "== Mao da Máquina ========"
       imprimir novoBaralhoJogadorMaquina
     else do
      putStrLn $ "== Mao da Máquina ========"
      imprimirMaquina novoBaralhoJogadorMaquina 1
  
  if (novaPontuacaoJogadorHumano >= 21 || novaPontuacaoJogadorMaquina >= 21) then vencedorPartida novaPontuacaoJogadorHumano novaPontuacaoJogadorMaquina
  else do 
  putStrLn("Deseja puxar outra carta? 1 - Sim | 2 - Não")

  escolhaTemp <- getLine
  let escolha = read escolhaTemp :: Int
  
  --Atualizando índice
  let novoTamanhoBaralho = (tamanhoBaralho - 2)

  puxarCarta escolha novoTamanhoBaralho novaPontuacaoJogadorHumano novaPontuacaoJogadorMaquina novoBaralhoJogadorHumano novoBaralhoJogadorMaquina baralhoFinal

imprimir :: [(String,Int)] -> IO()
imprimir mao = do
  if ((length mao) == 0) then putStrLn("======================")
  else do
    putStrLn ("|  - " ++ (fst (head mao)))
    imprimir (tail mao)

imprimirMaquina :: [(String,Int)] -> Int -> IO()
imprimirMaquina mao flag = do
  if ((length mao) == 0) then putStrLn("======================")
  else do
    if (flag == 1) then do
      putStrLn ("|  - " ++ (fst (head mao)))
      imprimirMaquina (tail mao) 0
    else do
      putStrLn ("|  - ???")
      imprimirMaquina (tail mao) 0

