:-use_module(library(random)).
:-use_module(library(lists)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%   GLOBALS  %%%%%%%%%%%%%%%%%%%%%%%%%%%

tamanho_tabuleiro(13). 
mid_linha([r,b,b,b,b,b,b,b,b,b,b,b]).           %b: espa�o em branco
tab([[]]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%% Funcao main %%%%%%%%%%%%%%%%%%%%%%%%
main:-
       tamanho_tabuleiro(N),  tab(Tab),   mid_linha(M),    
       completa_top_line(N,P),
       append(Tab,[P],T),               
       completa_mid_aux(11,M,O),
       append(T,O,X),
       removehead(X,L), %remove 1� elemento (lista vazia -> efeito remeniscente)
       completa_bot_line(L,A),
       append(L,[A],K),
       imprime_tab(K).
             
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
removehead([_|Tail], Tail).
   

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
completa_top_line(0, []).

completa_top_line(N, L) :-
        N > 0,
        N1 is N-1,
        random_gen(X),
        completa_top_line(N1, L1),
        append(L1, [X], L).


completa_mid_aux(0,_,_). %caso base

completa_mid_aux(N,L,P):-
        N > 0,
        N1 is N-1,
        completa_mid_line(L,E),       
        completa_mid_aux(N1,L,X1),
        append(X1,[E],P).
                       

%adiciona valor random no inicio da lista e o seu inverso no final da lista [0...1] ou [1....0]
completa_mid_line([_|T],E):-  % _E corresponde � lista a retornar pela func��o
         random_gen(X),
         H is X,
         append([H],T,L),
         inverte(X,V),
         append(L,[V],E).       


completa_bot_line([H|_],P):-
        invert_aux(H,X),
        reverse(X,P).    %>>>>>>TODO: CORRIGIR BUG invert_aux depois remover esta chamada : reverse(...
        

invert_aux([H|T],P):-
        inverte(H,X),
        invert_aux(T,P1),  
        append(P1,[X],P).  %TODO>>>se trocar para append([X],P1,P) devolve list na ordem certa mas com elemento a +, ex: [1,0,1|_A]

invert_aux([],_).              % caso base         
       
inverte(0,X):-  X is 1.
inverte(1,X):-  X is 0.   

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%        
%%%%%%%%%%%%%%%%%% IMPRESS�O DO TABULEIRO %%%%%%%%%%%%%%%%%%%%%%%

imprime_tab(Tab):-
        imprime_linhas(Tab).
        
 
imprime_linhas([Head|Tail]):-
        imprime_aux(Head),write('|'),nl,
        imprime_linhas(Tail).
 
imprime_linhas([]).      

/*fun�ao auxiliar que imprime no ecra*/
imprime_aux([H|T]):-
        mostra(H), %write(b),
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
        write('| ').
mostra(0):-
        write('|X').
mostra(1):-
        write('|O').

random_gen(V):-
        random(0,2,V).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

