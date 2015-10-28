:-use_module(library(random)).
:-use_module(library(lists)).

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

replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > -1, NI is I-1, replace(T, NI, X, R), !.
replace(L, _, _, L).

fillTabuleiro(Size,Tab):-
       generateEmptyLine(Size,b,Line),          %Lista Random
       generateEmptyLine(Size,Line,Tab1),       %Matriz
       generateRandomLine(Size,Inicio),         %Primeira Fila
       reverseLine(Inicio,Inverso),             %Ultima Fila
       generateRandomLine(Size,Lateral),         
       reverseLine(Lateral,InversoLateral),
       generateSide(Size,Tab1,Lateral,InversoLateral,Tab2),
       replace(Tab2,0,Inicio,Tab3),             %substitui primeira fila
       Ultimo is Size-1,
       replace(Tab3,Ultimo,Inverso,Tab).        %substitui ultima fila

  
generateSide(Size,[Lista|TTab],[HLateral|TLateral],[HLInverso|TInverso],[NovaLista1|NovaTab]):-
        Size>0,
        N1 is Size-1,
        generateSide(N1,TTab,TLateral,TInverso,NovaTab),
        length(Lista,Tamanho),
        Tamanho1 is Tamanho -1,     
        replace(Lista,Tamanho1,HLInverso,NovaLista),
        replace(NovaLista,0,HLateral,NovaLista1).
        
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
%%%%%%%%%%%%%%%%%% IMPRESSÃO DO TABULEIRO %%%%%%%%%%%%%%%%%%%%%%%

line_graphic(['    ',' A ', ' B ',' C ',' D ',' E ' ,' F ',' G '
             ,' H ',' I ',' J ',' K ',' L ',' M ']).  

imprime_tab(Tab):-
        line_graphic(L),
        imprime_aux(L), nl, 
        write('   '),     
        imprime(13*3+3,['-']), nl,
        imprime_linhas(Tab,1).

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


/* menu */
logo :-  nl, write('***   MORRELLI  ***'), nl, nl.

start :- repeat, write('\33\[2J'), nl, logo, write(' ---- MENU ----'), nl, nl,
                        write('1. Player vs Player'),nl, 
                        write('2. Player vs Computer'),nl, 
                        write('3. Computer vs Computer'),nl, 
                        write('4. Exit'), nl, nl,
                        write('Write the number of the option followed by a dot.'), nl,
                        read(C), C>0, C=<4, number(C), choice(C).

/* Menu Options */
choice(1) :- main.
choice(2) :- main.
choice(3) :- main.
choice(4) :- abort.
