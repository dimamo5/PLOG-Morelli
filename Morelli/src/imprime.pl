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

                %% TODO !!!!!!!!!!!!!!!!!!!
%%%%%% ALTERAR GERA�AO DAS LINHAS PARA ESTAREM DE ACORDO COM AS REGRAS
%%%%%% ULTIMA LINHA = invert_color(reverse(1� LINHA))
%%%%%% LINHA DA DIREITA = invert_color(reverse(LINHA da esquerda))

main:-
       tamanho_tabuleiro(N),  tab(Tab),   mid_linha(M),    
       completa_top_line(N,P),
       append(Tab,[P],T),               
       completa_mid_aux(11,M,O),
       append(T,O,X),
       removehead(X,L), %remove 1� elemento (lista vazia -> efeito remeniscente)
       completa_bot_line(L,A),
       append(L,[A],K),
       write(K).
     %  imprime_tab(K).
             
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

line_graphic(['    ','   A  ', '   B  ','   C  ','   D  ','   E  ' ,'   F  ','   G  '
             ,'   H  ','   I  ','   J  ','   K  ','   L  ','   M  ']).  

imprime_tab(Tab):-
        line_graphic(L),
        imprime_aux(L), nl, 
        write('   '),     
        imprime(13*6+3,['-']), nl,
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
         write('   '), imprime(13*6+3,['-']), nl,
        imprime_linhas(Tail,N1).

 
imprime_linhas([],_).

/*fun�ao auxiliar que imprime no ecra*/
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
        write('   .  ').
mostra(0):-
        write('   x  ').
mostra(1):-
        write('   o  ').

mostra(X):-
        write(X).

random_gen(V):-
        random(0,2,V).
