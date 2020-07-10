      *Divis�o de identifica��o do programa
       identification division.
      *---program-id � uma informa��o obrigat�ria---
       program-id. "desafioPizza".
       author. "Camila da Rosa Hostin".
       installation. "PC".
       date-written. 07/07/2020.
       date-compiled. 10/07/2020.

      *Divis�o para configura��o de ambiente
       environment division.
       configuration Section.
           special-names. decimal-point is comma.

      *Declara��o de recursos externos
       input-output section.
       file-control.
       i-o-control.

      *Declara��o de vari�veis
       data division.
      *-data division, tem 4 sess�es poss�veis-
      *---vari�veis de arquivos---
       file section.
      *---vari�veis de trabalho---
       working-storage section.

       77 ind                                      pic 9(4).
       77 menu                                     pic x(1).
       77 controle                                 pic x(10).
       77 pi                                       pic 9(1)v99.
       77 aux                                      pic 9(10).
       77 nomeAux                                  pic a(10).
       77 qtdPizza                                 pic 9(2).

      *   vari�veis da tabela
       01 relatorio occurs 20.
           05 nome                                 pic x(15).
           05 filler                               pic x(3) value ' | '.
           05 diametro                             pic 9(3).
           05 filler                               pic x(3) value ' | '.
           05 preco                                pic 9(3)v99.
           05 filler                               pic x(3) value ' | '.
           05 areapizza                             pic 9(3)v99.
           05 filler                               pic x(3) value ' | '.
           05 preco_cm2                            pic 9(3)v99.
           05 filler                               pic x(3) value ' | '.
           05 diferenca                            pic 9(3).
           05 filler                               pic x(3) value ' | '.
           05 porcentagem                          pic 9(3).
           05 filler                               pic x(1) value '%'.

      *---vari�veis para comunica��o entre programas---
       linkage section.
      *---declara��o de tela---
       screen section.
      *---------------- apresenta��o do problema -----------------------*
      *    Uma empresa de pesquisas online solicitou o desenvolvimento
      *de um software capaz de identificar qual tamanho de pizza
      *apresenta o melhor custo beneficio.
      *    O software dever� receber diversos tamanhos de pizza e seus
      *respectivos pre�os e ao final exibir um relat�rio informando em
      *valores absolutos e relativos (percentual) qual a diferen�a de
      *pre�os entre as pizzas e dever� informar qual pizza tem o melhor
      *custo benef�cio.
      *    Entradas:  1. Nome comercial (broto, baby, pequena, m�dia,
      *grande, exagerada, gigante, etc), o tamanho da pizza (di�metro
      *em cent�metros) e respectivo  pre�o. O software dever� aceitar
      *tantas entradas quanto o usu�rio deseja comparar, desde que n�o
      *haja tamanhos duplicados.
      *    Sa�da: relat�rio contendo todos os nomes e tamanhos de pizza
      *ordenados do melhor para o pior custo benef�cio.  O relat�rio
      *dever� informar o percentual  de diferen�a do pre�o de um
      *tamanho para o outro.
      *-----------------------------------------------------------------*

      *Declara��o do corpo do programa
       Procedure Division.

      *    estrutura programa
           perform inicializa.
           perform processamento.
           perform finaliza.

       inicializa section.

      *    inicializando as vari�veis
           move 1 to ind
           move 'S' to menu
           move 3,14 to pi
           move 0 to areaPizza(ind)
           .
       inicializa-exit.
           exit.

       processamento section.
      *corpo do programa da pizzaria

      *    inicializando as vari�veis
           move 0 to ind
           move 0 to qtdPizza

      *    perform para come�ar a entrada de dados
           perform until menu <> 'S'
               display erase

      *        inicializando as vari�veis
               add 1 to ind

      *        mostrar que o usu�rio n�o pode cadastrar + de 20 pizzas
               if ind > 20 then
                   display 'Voce Atingiu o Limite de 20 Pizzas'
               else
                   add 1 to qtdPizza
      *            cadastrando as pizzas
                   display 'Informe o Nome da Pizza: '
                   accept nome(ind)
                   display 'Informe o Diametro da Pizza: '
                   accept diametro(ind)
                   display 'Informe o Preco da Pizza: '
                   accept preco(ind)
               end-if

      *        chamando a section para calcular a �rea da pizza
               perform calc-area
      *        chamando a section para calcular o pre�o por cm2
               perform calc-precocm2

      *        menu para saber se pessoa quer continuar
               display 'Deseja Cadastrar mais uma Pizza? S/N'
               accept menu

           end-perform
      *        chamando a section de ordena��o
               perform ordena
      *        chamando a section de c�lculo da porcentagem
               perform calc-porcentagem

      *    apresentando a tabela
           perform varying ind from 1 by 1 until ind > 20
           or nome(ind) = space
               display relatorio(ind)
           end-perform
           .
       processamento-exit.
           exit.
      *-----------------------------------------------------------------*
       calc-area section.
      * calculo da area
           compute areapizza(ind) = pi * ((diametro(ind) / 2)
                                  * (diametro(ind) / 2))
           .
       calc-area-exit.
           exit.
      *-----------------------------------------------------------------*
       calc-precocm2 section.
      * calculo do pre�o por cm2
           compute preco_cm2(ind) = areapizza(ind) / preco(ind)
           .
       calc-precocm2-exit.
           exit.
      *-----------------------------------------------------------------*
       ordena section.
      *organizando a tabela por custo benef�cio

      *    inicializando as vari�veis
           move 'trocou' to controle
           perform until controle <> 'trocou'

      *        inicializando as vari�veis
               move 1 to ind
               move 'N_trocou' to controle
               perform until ind = qtdPizza
                   if preco_cm2(ind) > preco_cm2(ind + 1)
      *                organizando a vari�vel pre�os_cm2
                       move preco_cm2(ind + 1) to aux
                       move preco_cm2(ind) to preco_cm2(ind + 1)
                       move aux to preco_cm2(ind)

      *                organizando a vari�vel nome
                       move nome(ind + 1) to nomeAux
                       move nome(ind) to nome(ind + 1)
                       move nomeAux to nome(ind)

      *                organizando a vari�vel di�metro
                       move diametro(ind + 1) to aux
                       move diametro(ind) to diametro(ind + 1)
                       move aux to diametro(ind)

      *                organizando a vari�vel preco
                       move preco(ind + 1) to aux
                       move preco(ind) to preco(ind + 1)
                       move aux to preco(ind)

      *                organizando a vari�vel areapizza
                       move areapizza(ind + 1) to aux
                       move areapizza(ind) to areapizza(ind + 1)
                       move aux to areapizza(ind)

      *                para continuar fazendo a organiza��o
                       move 'trocou' to controle
                   end-if
                   add 1 to ind
               end-perform
           end-perform
           .
       ordena-exit.
           exit.
      *-----------------------------------------------------------------*
       calc-porcentagem section.
      *fazer o c�lculo da porcentagem de qual ser� o melhor pre�o

      *    inicializando as vari�veis
           move 1 to ind
           move 0 to porcentagem(ind)
           move 0 to diferenca(ind)

               perform until ind > qtdPizza - 1
      *            c�lculo de diferen�a de pre�os
                   compute diferenca(ind + 1) = preco_cm2(ind + 1)
                                              - preco_cm2(ind)
      *            c�lculo de porcentagem
                   compute porcentagem(ind + 1) =
                           (diferenca(ind + 1) * 100) / preco_cm2 (ind)
                   add 1 to ind
               end-perform
           .
       calc-porcentagem-exit.
           exit.
      *-----------------------------------------------------------------*
       finaliza section.
      *fazendo a finaliza��o do programa

           stop run
           .
       finaliza-exit.
           exit.

