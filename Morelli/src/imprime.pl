estado_inicial([[random_gen,random_gen,random_gen,random_gen,random_gen],
               [random_gen,' ',' ',' ',random_gen],
               [random_gen,' ',' ',' ',random_gen],
               [random_gen,' ',' ',' ',random_gen]]).
      
load:-use_module(library(random)).

cenas:-
        estado_inicial(Tab),
        imprime_tab(Tab).


imprime_tab(Tab):-
        imprime_linhas(Tab).
        
 
imprime_linhas([Head|Tail]):-
        imprime_aux(Head),nl,
        imprime_linhas(Tail).       


/* imprime uma determinada lista X, N vezes */
imprime(N,X):-
        N > 1,
        N1 is N-1,
        imprime_aux(X),
        imprime(N1,X).
 
imprime(1,_). /* caso base */    

/*fun�ao auxiliar que imprime no ecra*/
imprime_aux([H|T]):-
        mostra(H), write(' '),
        imprime_aux(T).

imprime_aux([]). /* caso base */

mostra(random_gen):-
       random_gen(X),
       write(X).

mostra(' '):-
        write(' ').

random_gen(V):-
        random(0,2,V).

