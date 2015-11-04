:-use_module(library(random)).
:-use_module(library(lists)).
:-use_module(library(between)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%   GLOBALS  %%%%%%%%%%%%%%%%%%%%%%%%%%%

tamanho_tabuleiro(13). 
mid_linha([r,b,b,b,b,b,b,b,b,b,b,b]).           %b: espaço em branco
tab([[]]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%% Funcao main %%%%%%%%%%%%%%%%%%%%%%%%

                %% TODO !!!!!!!!!!!!!!!!!!!
%%%%%% ALTERAR GERAÇAO DAS LINHAS PARA ESTAREM DE ACORDO COM AS REGRAS
%%%%%% ULTIMA LINHA = invert_color(reverse(1ª LINHA))
%%%%%% LINHA DA DIREITA = invert_color(reverse(LINHA da esquerda))

main:-
       fillTabuleiro(13,Tab),
       imprime_tab(Tab).
             
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
removehead([_|Tail], Tail).
   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

replace([_|T], 1, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > 0, NI is I-1, replace(T, NI, X, R), !.
replace(L, _, _, L).

fillTabuleiro(Tab,Size):-
       generateEmptyLine(Size,b,Line),          %Lista Random
       generateEmptyLine(Size,Line,Tab1),       %Matriz
       generateRandomLine(Size,Inicio),         %Primeira Fila
       reverseLine(Inicio,Inverso),             %Ultima Fila
       generateRandomLine(Size,Lateral),         
       reverseLine(Lateral,InversoLateral),
       generateSide(Size,Tab1,Lateral,InversoLateral,Tab2),
       replace(Tab2,1,Inicio,Tab3),             %substitui primeira fila
       replace(Tab3,Size,Inverso,Tab),!.        %substitui ultima fila

  
generateSide(Size,[Lista|TTab],[HLateral|TLateral],[HLInverso|TInverso],[NovaLista1|NovaTab]):-
        Size>0,
        N1 is Size-1,
        generateSide(N1,TTab,TLateral,TInverso,NovaTab),
        length(Lista,Tamanho),    
        replace(Lista,Tamanho,HLInverso,NovaLista),
        replace(NovaLista,1,HLateral,NovaLista1).
        
generateSide(0,[],[],[],[]).        
             

generateEmptyLine(Size,Elem,List):-
         Size > 0,
         N1 is Size-1,
         generateEmptyLine(N1,Elem,List1),
         append(List1,[Elem],List).
         

generateEmptyLine(0,_,[]).

generateRandomLine(0,[]).

generateRandomLine(Size,Line):- 
        Size > 0,
        Size1 is Size-1,
        random_gen(X),
        generateRandomLine(Size1, Line1),
        append(Line1, [X], Line).


reverseLine(L,L1):-
        reverse(L,ListaInvertida),
        reverseLineAux(ListaInvertida,L1).

reverseLineAux([],[]).

reverseLineAux([H|T],[X|P]):-
        inverte(H,X),
        reverseLineAux(T,P).  
          %TODO>>>se trocar para append([X],P1,P) devolve list na ordem certa mas com elemento a +, ex: [1,0,1|_A]

              % caso base         
       
inverte(0,1).
inverte(1,0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%        
%%%%%%%%%%%%%%%%%% LOGICA %%%%%%%%%%%%%%%%%%%%%%%

chooseMove(Board,Size,Player,NewBoard):- write('Que peca deseja mover? Jogador '),((Player =:= 0, write('Jogador X'));(Player =:= 1,write('Jogador O'))),nl,write('X = '), read(X),nl, write('Y = '), read(Y),nl,
                                                                                canUsePiece(Board,X,Y,Player),
                                                                                write('New X = '), read(XF),nl, write('New Y = '),read(YF),nl,
                                                                                validMove(Board,Size,X,Y,XF,YF,Player), 
                                                                                movePiece(Board,X,Y,XF,YF,NewBoard2),
                                                                                checkTrone(Board,Size,XF,YF,Player,NewBoard),
                                                                                printTable(NewBoard2,XF,YF),
                                                                                chooseMove(NewBoard2,Player,NewBoard,NewestCost),!.
                                                                                
chooseMove(Board,Player,NewBoard):-write('Jogada Impossivel'),nl,nl,chooseMove(Board,Player,NewBoard),!.  %caso falhe

%modifica o trono se for necessario
checkTrone(Board,Size,XF,YF,Player,NewBoard):-
      C is round(Size/2),
      DiffX is XF-C,
      DiffY is YF-C,
      X1 is -DiffX+C,
      Y1 is DiffY+C,
      X2 is DiffX+C,
      Y2 is -DiffY+C,
      X3 is -DiffX+C,
      Y3 is -DiffY+C,
      positionValue(Board,X1,Y1,Player),
      positionValue(Board,X2,Y2,Player),
      positionValue(Board,X3,Y3,Player),
      write('Did it boys').
        
getPiecesFrame([X,Y],Size,Tamanho):-       
        C is round(Size/2),
        frame(X,Y,C,C,Tamanho).   
        


%Verifica se é possivel efectuar um movimento
validMove(Board,Size,X,Y,XF,YF,Player):-
        %TODO: ver prox lina falha qd resultado = no
        \+ pontosIguais(X,Y,XF,YF),                             % PosInicial != posFinal
        freeSpace(Board,XF,YF),                                                        % nao existe peca na Pos final
        declive_recta(X,Y,XF,YF),                               % para ser diagonal declive = 1
        verifyFrame(X,Y,XF,YF,Size),             % verifica se esta a mover para o centro   
        checkFreeWay(Board,X,Y,XF,YF).                          % tem caminho livre para Pos final


%Mover a peca
movePiece(Board,X,Y,XF,YF,NewBoard2):-
        positionValue(Board,X,Y,V),
        replaceElemMatrix(Board,X,Y,b,Board1),
        replaceElemMatrix(Board1,XF,YF,V,Board2),
        findall(List,listPieceCapture(Board2,3,3,List),Bag),
        capturePiece(Board2,V,Bag,NewBoard2).

%Lista todas as pecas a captura
listPieceCapture(Board,X,Y,List):-
        positionValue(Board,X,Y,V),
        inverte(V,Adv),
        between(-1,1,DiffX),
        NewX is X+DiffX,
        between(-1,1,DiffY),
        NewY is Y+DiffY,
        positionValue(Board,NewX,NewY,V1),
        V1=Adv,
        NewX2 is NewX+DiffX,
        NewY2 is NewY+DiffY,
        positionValue(Board,NewX2,NewY2,V),
        List = [NewX,NewY].

%Captura as pecas
capturePiece(Board,_,[],Board).

capturePiece(Board,Player,[H|T],Board2):-
        nth0(0,H,X),
        nth0(1,H,Y),
        replaceElemMatrix(Board,X,Y,Player,NewBoard),
        capturePiece(NewBoard,Player,T,Board2).
        

replaceElemMatrix(Board,X,Y,Elem,NewBoard):-
        nth1(Y,Board,Lista),
        replace(Lista,X,Elem,NovaLista),
        replace(Board,Y,NovaLista,NewBoard).              
        

% verifica se 2 pontos sao iguais
pontosIguais(X,Y,XF,YF):- X =:= XF, Y =:= YF.    

%verifica se o elemento 'Player' existe na coo (X,Y)
canUsePiece(Board,X,Y,Player):-nth1(Y,Board,Linha),nth1(X,Linha,Peca),Peca =:= Player.

%devolve valor de uma posicao no tabuleiro (r,b,1,0)..
positionValue(Board,X,Y,V):- nth1(Y,Board,Linha),nth1(X,Linha,V).

%verifica se pos(X,Y) está livre
freeSpace(Board,X,Y):-nth1(Y,Board,Linha),nth1(X,Linha,Peca),Peca = b. 

%verifica no caso de segmento de recta ser obliquo: se declive = 1 ou -1 -> movimento diagonal
declive_recta(X,Y,XF,YF):- X =:= XF.                    %seg recta vertical
declive_recta(X,Y,XF,YF):- ((YF-Y)/(XF-X)) =:= 0.       %seg recta horizontal
declive_recta(X,Y,XF,YF):- ((YF-Y)/(XF-X)) =:= 1.       %seg recta diagonal


%verifica se esta a mover para o centro    
verifyFrame(X,Y,XF,YF,Size):-
        C is round(Size/2),
        frame(XF,YF,C,C,F1),    %frame ponto final
        frame(X,Y,C,C,F2),      %frame ponto inicial
        F2 > F1,
        F1 =\= 0.                  %impede jogada em que posFinal = posCentral

       
%frame de 1 ponto = distancia do centro ao ponto
frame(X1,Y1,X2,Y2,R):-
                R is sqrt(exp(X2-X1,2) + exp(Y2-Y1,2)). 
           

%verifica se nao existe nenhum elemento entre pos inicial e pos final
checkFreeWay(Board,X,Y,XF,YF):-
        decremento(X,Y,XF,YF,DX,DY),            % devolve decremento/incremento a ser aplicado
        checkFreeWayAux(Board,X,Y,XF,YF,DX,DY). % itera ate PosActual == PosFinal

checkFreeWayAux(Board,X,Y,X2,Y2,DX,DY):-pontosIguais(X,Y,X2,Y2).

checkFreeWayAux(Board,X,Y,X2,Y2,DX,DY):-
        \+ pontosIguais(X,Y,X2,Y2),          % enquanto nao chegar a posFinal continua
        NewX is X + DX,                     
        NewY is Y + DY,
        freeSpace(Board,NewX,NewY),          % espaco esta livre
        checkFreeWayAux(Board,NewX,NewY,X2,Y2,DX,DY).
 
%devolve decremento/incremento a aplicar nas coos para percorrer seg.recta [PoxI,PosF]        
decremento(X,Y,XF,YF,DX,DY):-           %deslocamento diagonal
        X =\= XF,
        Y =\= YF,
        DX is integer((XF-X)/abs(XF-X)),
        DY is integer((YF-Y)/abs(YF-Y)).

decremento(X,Y,XF,YF,DX,DY):-           %deslocamento horizontal
        X < XF,
        DY is 0,
        DX is integer((XF-X)/abs(XF-X)).

decremento(X,Y,XF,YF,DX,DY):-           %deslocamento vertical
        Y < YF,
        DX is 0,
        DY is integer((YF-Y)/abs(YF-Y)).
        

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%        
%%%%%%%%%%%%%%%%%% IMPRESSÃO DO TABULEIRO %%%%%%%%%%%%%%%%%%%%%%%

line_graphic(['    ',' A ', ' B ',' C ',' D ',' E ' ,' F ',' G '
             ,' H ',' I ',' J ',' K ',' L ',' M ']).  

imprime_tab(Tab):-
        line_graphic(L),
        imprime_aux(L), nl, 
        write('   '),     
        imprime(13*3+3,['-']), nl,
        imprime_linhas(Tab,1),!.

aux(N):-
        N < 10,
        write(N),
        write('  |').
aux(N):-
        write(N),
        write(' |').
        
 
imprime_linhas([Head|Tail],N):-   %descomentar em baixo para aspecto visual diferente
        N1 is N+1,
        aux(N),
        imprime_aux(Head),  write(' |'),nl,
         write('   '), imprime(13*3+3,['-']), nl,
        imprime_linhas(Tail,N1).

 
imprime_linhas([],_).

/*funçao auxiliar que imprime no ecra*/
imprime_aux([H|T]):-
        mostra(H),
        imprime_aux(T).

imprime_aux([]). /* caso base */



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/* imprime uma determinada lista X, N vezes */
imprime(N,X):-
        N > 1,
        N1 is N-1,
        imprime_aux(X),
        imprime(N1,X).
 
imprime(1,_). /* caso base */  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mostra(r):-
       random_gen(X),
       mostra(X).

mostra(b):-
        write(' . ').
mostra(0):-
        write(' x ').
mostra(1):-
        write(' o ').

mostra(X):-
        write(X).

random_gen(V):-
        random(0,2,V).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%     
                        /* menu */
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%     


logo :-  nl, write('***   MORRELLI  ***'), nl, nl.

start :- repeat, nl, logo, write(' ---- MENU ----'), nl, nl,
                        write('1. Player vs Player'),nl, 
                        write('2. Player vs Computer'),nl, 
                        write('3. Computer vs Computer'),nl, 
                        write('4. Exit'), nl, nl,
                        write('Write the number of the option followed by a dot.'), nl,
                        read(C), C>0, C=<4, number(C), choice(C).

/* Menu Options */
%choice(1) :- fillTabuleiro(13,Tab),chooseMove(Tab,
choice(2) :- read(C),write(C).
choice(3) :- main.
choice(4) :- abort.
