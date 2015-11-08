:-use_module(library(random)).
:-use_module(library(lists)).
:-use_module(library(between)).

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

%caso especifico: jogador nao tem mais movimentos possiveis e tem o trono, no entanto o outro jogador 
%com 1 movimento consegue ocupar o trono, como proceder? Terminar assim que 1 jogador n tenha mais jogadas possiveis ?


%LOOP DE JOGO
play(Board,Size,0,Bot1,Bot2):-continueGame(Board,Size,0), whoPlays(Board,Size,0,NewBoard,Bot1,Bot2),!,play(NewBoard,Size,1,Bot1,Bot2).
play(Board,Size,1,Bot1,Bot2):-continueGame(Board,Size,1), whoPlays(Board,Size,1,NewBoard,Bot1,Bot2),!,play(NewBoard,Size,0,Bot1,Bot2).


play(Board,Size,_,_,_):-verificaVencedor(Board,Size,Winner),vencedor(Winner).

%Player vs Player
whoPlays(Board,Size,Player,NewBoard,0,0):-chooseMove(Board,Size,Player,NewBoard).

%Player vs Bot level 1
whoPlays(Board,Size,1,NewBoard,0,1):-randomMove(Board,Size,1,NewBoard).
whoPlays(Board,Size,0,NewBoard,0,1):-chooseMove(Board,Size,0,NewBoard).

%Player vs Bot level 2
whoPlays(Board,Size,1,NewBoard,0,2):-smartMove(Board,Size,1,NewBoard).
whoPlays(Board,Size,0,NewBoard,0,2):-chooseMove(Board,Size,0,NewBoard).

%Bot level 1 vs Bot level 1
whoPlays(Board,Size,Player,NewBoard,1,1):-randomMove(Board,Size,Player,NewBoard).

%Bot level 1 vs Bot level 2
whoPlays(Board,Size,1,NewBoard,1,2):-smartMove(Board,Size,1,NewBoard).
whoPlays(Board,Size,0,NewBoard,1,2):-randomMove(Board,Size,0,NewBoard).

%Bot level 2 vs Bot level 1
whoPlays(Board,Size,1,NewBoard,2,1):-randomMove(Board,Size,1,NewBoard).
whoPlays(Board,Size,0,NewBoard,2,1):-smartMove(Board,Size,0,NewBoard).

%Bot level 2 vs Bot level 2
whoPlays(Board,Size,Player,NewBoard,2,2):-smartMove(Board,Size,Player,NewBoard).
    
    
smartMove(Board,Size,Player,NewBoard):-
        findall([X,Y],positionValue(Board,X,Y,Player),PiecesPlayer),
        getAllMoves(PiecesPlayer,Board,Size,AllMoves),
        evaluate_and_choose(Board,Size,AllMoves, Player ,-9999, BestMove,BestMoveF),
        nth0(0,BestMoveF,X1),nth0(1,BestMoveF,Y1),nth0(2,BestMoveF,XF1),nth0(3,BestMoveF,YF1),
        movePiece(Board,X1,Y1,XF1,YF1,NewBoard1,PecasAlteradas),
        changeTrone(NewBoard1,Size,PecasAlteradas,Player,NewBoard),
        imprimeTab(NewBoard,Size),nl,write(X1),write(' '),write(Y1),write(' '),write(XF1),write(' '),write(YF1),nl,!.


evaluate_and_choose(_,_,[], _ ,Score, BestMove,BestMove).%:- write('Score final: '),write(Score),write('  Move:'),write(BestMove).

evaluate_and_choose(Board,Size,[[X,Y,XF,YF] | T], Player ,Score, BestMove,BestMoveF):-
                                movePiece(Board,X,Y,XF,YF,NewBoard,PecasAlteradas),
                                evaluateBoard(NewBoard,Size,Player,Value,PecasAlteradas,[X,Y,XF,YF]),
                                %write('Score Antes: '),write(Value),
                                update(Score, Value,Value1,BestMove,[X,Y,XF,YF], BestMove1),
                                %write('Score Depois: '),write(Value1),write('  Move:'),write(BestMove1),
                                evaluate_and_choose(Board,Size,T, Player ,Value1, BestMove1,BestMoveF).

update(Score,Value,Score,OldMove,_,OldMove):- Score > Value.
update(Score,Value,Value,_,BestMove,BestMove):- Score =< Value. %Falha e estou farto que isto de paido fds

        
evaluateBoard(Board,Size,Player,Value,_,_):-  \+(continueGame(Board,Size,Player)),Value is 10000,write('Ganha: '),write(Value).
evaluateBoard(Board,Size,Player,Value,PecasAlteradas,[X,Y,XF,YF]):- checkTrone(Board,Size,XF,YF,Player,_),Value is 5000,write('Fica Trono: '),write(Value).
evaluateBoard(Board,_,Player,Value,_,[X,Y,XF,YF]):- numberOfPieces(Board,_,Player,Number),frame(X,Y,XF,YF,R),Value is 100*Number+(10*R)/2.
evaluateBoard(_,_,_,Value,_,[X,Y,XF,YF]):-frame(X,Y,XF,YF,R),Value is R*10,write('=======ERRO========').


numberOfPieces(Board,_,Player,Number):-
        findall([X,Y],positionValue(Board,X,Y,Player),Bag),
        length(Bag,Number).

randomMove(Board,Size,Player,NewBoard):-
        findall([X,Y],positionValue(Board,X,Y,Player),Bag),
        getAllMoves(Bag,Board,Size,Res),
        length(Res,Tamanho),
        random(0,Tamanho,V),
        nth0(V,Res,Mov),
        nth0(0,Mov,X),nth0(1,Mov,Y),nth0(2,Mov,XF),nth0(3,Mov,YF),
        movePiece(Board,X,Y,XF,YF,NewBoard2,PecasAlteradas),
        changeTrone(NewBoard2,Size,PecasAlteradas,Player,NewBoard),
        imprimeTab(NewBoard,Size),nl,nl,!.

imprimeTrone(Board,Size):-
        C is round(Size/2),
        positionValue(Board,C,C,Winner),
        write(Winner),nl.
        

continueGame(Board,Size,Player):-
        findall([X,Y],positionValue(Board,X,Y,Player),Bag),
        getAllMoves(Bag,Board,Size,Res),
        length(Res,Tamanho),Tamanho>1.
        


getAllMoves([],_,_,[]).

getAllMoves([H|T],Board,Size,Res):-
        nth0(0,H,X),
        nth0(1,H,Y),
        findall([X,Y,XV,YV],validMove(Board,Size,X,Y,XV,YV,_),Bag),
        getAllMoves(T,Board,Size,Res1),
        append(Res1,Bag,Res). 
        
                                   



verificaVencedor(Board,Size,Winner):-
        C is round(Size/2),
        positionValue(Board,C,C,Winner).            


vencedor(10):- write('Jogador O ganhou!').
vencedor(11):- write('Jogador X ganhou!').
vencedor(b):-  write('O jogo terminou com um empate!').
vencedor(_):- write('Erro').
        


playerTrone(1,11).
playerTrone(0,10).

checkInput(String,Size,X,Y):-
        name(String,Codes),
        nth0(0,Codes,X1),
        nth0(1,Codes,Y1),
        X is X1-96,
        Y is Y1-48,
        X>0,X=<Size,
        Y>0,Y=<Size.



chooseMove(Board,Size,Player,NewBoard):- write('Que peca deseja mover?'),((Player =:= 0, write('Jogador O'));(Player =:= 1,write('Jogador X'))),nl,
                                                                                write('Coords = '), read(Input),checkInput(Input,Size,X,Y),
                                                                                canUsePiece(Board,X,Y,Player),
                                                                                write('New Coords = '), read(InputF),checkInput(InputF,Size,XF,YF),nl,
                                                                                validMove(Board,Size,X,Y,XF,YF,Player), 
                                                                                movePiece(Board,X,Y,XF,YF,NewBoard2,PecasAlteradas),
                                                                                changeTrone(NewBoard2,Size,PecasAlteradas,Player,NewBoard),
                                                                                imprimeTab(NewBoard,Size),nl,nl,!.
                                                                                
chooseMove(Board,Size,Player,NewBoard):-write('Jogada Impossivel'),nl,nl,chooseMove(Board,Size,Player,NewBoard),!.  %caso falhe

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
      playerTrone(Player,Trone),
      replaceElemMatrix(Board,C,C,Trone,NewBoard),
      write([X1,Y1,X2,Y2,X3,Y3]).


changeTrone(Board,_,[],_,Board).

changeTrone(Board,Size,[H|T],Player,NewBoard):-
        nth0(0,H,X),
        nth0(1,H,Y),
        if(checkTrone(Board,Size,X,Y,Player,NewBoard),true,changeTrone(Board,Size,T,Player,NewBoard)).
              
        
generateMoves(Board,Size,X,Y,[XV,YV]):-
        between(1,Size,XV),between(1,Size,YV),validMove(Board,Size,X,Y,XV,YV,_).

imprimex([H|T]):-
        write(H),
        nl,
        imprimex(T).

%Verifica se ï¿½ possivel efectuar um movimento
validMove(Board,Size,X,Y,XF,YF,_):-
        between(1,Size,XF),
        between(1,Size,YF),
        \+ pontosIguais(X,Y,XF,YF),                             % PosInicial != posFinal
        freeSpace(Board,XF,YF),                                                        % nao existe peca na Pos final
        declive_recta(X,Y,XF,YF),                               % para ser diagonal declive = 1
        verifyFrame(X,Y,XF,YF,Size),             % verifica se esta a mover para o centro   
        checkFreeWay(Board,X,Y,XF,YF).                          % tem caminho livre para Pos final


%Mover a peca
movePiece(Board,X,Y,XF,YF,NewBoard2,PecasAlteradas):-
        positionValue(Board,X,Y,V),
        replaceElemMatrix(Board,X,Y,b,Board1),
        replaceElemMatrix(Board1,XF,YF,V,Board2),
        findall(List,listPieceCapture(Board2,XF,YF,List),Bag),
        append([[XF,YF]],Bag,PecasAlteradas),
        capturePiece(Board2,V,Bag,NewBoard2).

%Lista todas as pecas a capturar
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

%verifica se pos(X,Y) estï¿½ livre
freeSpace(Board,X,Y):-nth1(Y,Board,Linha),nth1(X,Linha,Peca),Peca = b. 

%verifica no caso de segmento de recta ser obliquo: se declive = 1 ou -1 -> movimento diagonal
declive_recta(X,_,XF,_):- X =:= XF,!.                    %seg recta vertical
declive_recta(X,Y,XF,YF):- ((YF-Y)/(XF-X)) =:= 0,!.       %seg recta horizontal
declive_recta(X,Y,XF,YF):- abs((YF-Y)/(XF-X)) =:= 1,!.       %seg recta diagonal


%verifica se esta a mover para o centro    
verifyFrame(X,Y,XF,YF,Size):-
        frame(XF,YF,Size,F1),    %frame ponto final
        frame(X,Y,Size,F2),      %frame ponto inicial
        F1>F2,
        F1 =\= round(Size/2).                  

frame(X,Y,Size,Level):-
        if(X>round(Size/2),ResX is Size-X+1,ResX is X),
        if(Y>round(Size/2),ResY is Size-Y+1,ResY is Y),
        Level is min(ResX,ResY).
         
%frame de 1 ponto = distancia do centro ao ponto
%frame(X1,Y1,X2,Y2,R):-
%                R is sqrt(exp(X2-X1,2) + exp(Y2-Y1,2)). 
           
%verifica se nao existe nenhum elemento entre pos inicial e pos final
checkFreeWay(Board,X,Y,XF,YF):-
        decremento(X,Y,XF,YF,DX,DY),            % devolve decremento/incremento a ser aplicado
        checkFreeWayAux(Board,X,Y,XF,YF,DX,DY). % itera ate PosActual == PosFinal

checkFreeWayAux(_,X,Y,X2,Y2,_,_):-pontosIguais(X,Y,X2,Y2).

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
        DY is integer((YF-Y)/abs(YF-Y)),!.

decremento(X,Y,XF,YF,DX,DY):-           %deslocamento horizontal
        Y=:=YF,
        DY is 0,
        DX is integer((XF-X)/abs(XF-X)),!.

decremento(X,Y,XF,YF,DX,DY):-           %deslocamento vertical
        X=:=XF,
        DX is 0,
        DY is integer((YF-Y)/abs(YF-Y)),!.
        

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%        
%%%%%%%%%%%%%%%%%% IMPRESSï¿½O DO TABULEIRO %%%%%%%%%%%%%%%%%%%%%%%
 
numberToLetter('A',1).
numberToLetter('B',2).
numberToLetter('C',3).
numberToLetter('D',4).
numberToLetter('E',5).
numberToLetter('F',6).
numberToLetter('G',7).
numberToLetter('H',8).
numberToLetter('I',9).
numberToLetter('J',10).
numberToLetter('K',11).
numberToLetter('L',12).
numberToLetter('M',13).

printHeader(0).

printHeader(Size):-
        N is Size -1,
        printHeader(N),
        numberToLetter(X,Size),
        write(' '),
        write(X),
        write('  ').

printLine(Size,Espace):-
        Half is (Size)/2,
        Espace<Half,
        imprime(Espace,['|   ']),imprime((Size-2*Espace)*4+1,['-']),imprime(Espace,['   |']).
        
printLine(Size,Espace):-
        Half is (Size)/2,
        Espace>Half,
        Espace1 is Size-Espace,
        imprime(Espace1,['|   ']),imprime((Size-2*Espace1)*4+1,['-']),imprime(Espace1,['   |']).
        
%imprime barras antes
printLineMatrix(Size,[H|T],BarraAntes,BarraDepois):-
        N is BarraAntes -1, BarraAntes>0,
        NSize is Size-1,Size>0,
        write('|'),mostra(H),
        printLineMatrix(NSize,T,N,BarraDepois),!.

%imprime elemnto unico
printLineMatrix(Size,[H|T],0,BarraDepois):-
        N is BarraDepois -1,
        Size>0,Size < BarraDepois,
        write('|'),
        printLineMatrix(Size,[H|T],0,N),!.

%imprime elemnto sem barras
printLineMatrix(Size,[H|T],0,BarraDepois):-
        N1 is Size -1,Size>0,Size =\= BarraDepois,
        write(' '),mostra(H),
        if(N1=BarraDepois,write(' '),true),
        printLineMatrix(N1,T,0,BarraDepois),!.

%imprime barras depois
printLineMatrix(Size,[H|T],0,BarraDepois):-
        N is BarraDepois -1, BarraDepois>0, Size=BarraDepois,
        NSize is Size-1,Size>0,
        mostra(H),write('|'),
        printLineMatrix(NSize,T,0,N),!.

printLineMatrix(0,[],0,0).

imprimeTab(Tab,Size):-
        write('    '),printHeader(Size),nl,
        imprimelinhas(Tab,Size,0),
        write('   '),printLine(Size,0),nl,
        write('    '),printHeader(Size),nl.

imprimelinhas([H|T],Size,N):-
        write('   '),printLine(Size,N),nl,
        N1 is N+1,
        N2 is Size-N,
        writeNumber(N1),write(' '),
        if(N<(Size/2),printLineMatrix(Size,H,N1,N1),printLineMatrix(Size,H,N2,N2)),
        writeNumber(N1),
        nl,imprimelinhas(T,Size,N1).

imprimelinhas([],_,_).        

/*funï¿½ao auxiliar que imprime no ecra*/
imprime_aux([H|T]):-
        mostra(H),
        imprime_aux(T).

imprime_aux([]). /* caso base */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/* imprime uma determinada lista X, N vezes */
imprime(N,X):-
        N > 0,
        N1 is N-1,
        imprime_aux(X),
        imprime(N1,X).
 
imprime(0,_). /* caso base */  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mostra(b):-
        write(' . ').
mostra(0):-
        write(' o ').
mostra(1):-
        write(' x ').
mostra(11):-
        write(' X ').
mostra(10):-
        write(' O ').
mostra(X):-
        write(X).

writeNumber(N):-N1 is N-10, N1>=0,write(N).
writeNumber(N):-write(' '),write(N).


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
choice(1) :- selectTamanho(Size),fillTabuleiro(Tab,Size),imprimeTab(Tab,Size),nl,play(Tab,Size,0,0,0).
choice(2) :- selectTamanho(Size),botDifficulty(Diff),fillTabuleiro(Tab,Size),imprimeTab(Tab,Size),nl,play(Tab,Size,0,0,Diff).
choice(3) :- selectTamanho(Size),botDifficulty(Diff1),botDifficulty(Diff2),fillTabuleiro(Tab,Size),imprimeTab(Tab,Size),nl,play(Tab,Size,0,Diff1,Diff2).
choice(4) :- abort.

selectTamanho(Tamanho):-write('Tamanho'),nl,read(Tamanho),Tamanho>0,Tamanho=<13,number(Tamanho),Tamanho mod 2 =:=1.
selectTamanho(Tamanho):-write('Tamanho Errado'),nl,selectTamanho(Tamanho),!.

botDifficulty(Diff):-write('Dificuldade do Bot (1-Fácil 2-Médio)'),nl,read(Diff),Diff>0,Diff=<2,number(Diff).
botDifficulty(Diff):-write('Dificuldade Errada'),nl,botDifficulty(Diff),!.
