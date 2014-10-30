%Autómatas de ejemplo. Si agregan otros,  mejor.

ejemplo(1, a(s1, [sf], [(s1, a, sf)])).
ejemplo(2, a(si, [si], [(si, a, si)])).
ejemplo(3, a(si, [si], [])).
ejemplo(4, a(s1, [s2, s3], [(s1, a, s1), (s1, a, s2), (s1, b, s3)])).
ejemplo(5, a(s1, [s2, s3], [(s1, a, s1), (s1, b, s2), (s1, c, s3), (s2, c, s3)])).
ejemplo(6, a(s1, [s3], [(s1, b, s2), (s3, n, s2), (s2, a, s3)])).
ejemplo(7, a(s1, [s2], [(s1, a, s3), (s3, a, s3), (s3, b, s2), (s2, b, s2)])).
ejemplo(8, a(s1, [sf], [(s1, a, s2), (s2, a, s3), (s2, b, s3), (s3, a, s1), (s3, b, s2), (s3, b, s4), (s4, f, sf)])). % No deterministico :)
ejemplo(9, a(s1, [s1], [(s1, a, s2), (s2, b, s1)])).
ejemplo(10, a(s1, [s10, s11], 
        [(s2, a, s3), (s4, a, s5), (s9, a, s10), (s5, d, s6), (s7, g, s8), (s15, g, s11), (s6, i, s7), (s13, l, s14), (s8, m, s9), (s12, o, s13), (s14, o, s15), (s1, p, s2), (s3, r, s4), (s2, r, s12), (s10, s, s11)])).
ejemplo(11, a(s1, [s4], [(s1, a, s2), (s1, b, s3), (s2, b, s4), (s3, b, s4), (s1, c, s4)])).

ejemploMalo(1, a(s1, [s2], [(s1, a, s1), (s1, b, s2), (s2, b, s2), (s2, a, s3)])). %s3 es un estado sin salida.
ejemploMalo(2, a(s1, [sf], [(s1, a, s1), (sf, b, sf)])). %sf no es alcanzable.
ejemploMalo(3, a(s1, [s2, s3], [(s1, a, s3), (s1, b, s3)])). %s2 no es alcanzable.
ejemploMalo(4, a(s1, [s3], [(s1, a, s3), (s2, b, s3)])). %s2 no es alcanzable.
ejemploMalo(5, a(s1, [s3, s2, s3], [(s1, a, s2), (s2, b, s3)])). %Tiene un estado final repetido.
ejemploMalo(6, a(s1, [s3], [(s1, a, s2), (s2, b, s3), (s1, a, s2)])). %Tiene una transición repetida.
ejemploMalo(7, a(s1, [], [(s1, a, s2), (s2, b, s3)])). %No tiene estados finales.

%%Proyectores
inicialDe(a(I, _, _), I).

finalesDe(a(_, F, _), F).

transicionesDe(a(_, _, T), T).

%Auxiliar dada en clase
%desde(+X, -Y).
desde(X, X).
desde(X, Y):-desde(X, Z),  Y is Z + 1.


%%Predicados pedidos.

% 1) %esDeterministico(+Automata)

hayTransicionRepetida(E1, Etiqueta, [(E1, Etiqueta, _)|_]).
hayTransicionRepetida(E1, Etiqueta, [(Ex, _, _)|Ls]) :- E1 \= Ex, hayTransicionRepetida(E1, Etiqueta, Ls).

transcionesDeterministicas([]).
transcionesDeterministicas([(E1, Etiqueta, _)|Ls]) :- transcionesDeterministicas(Ls), not(hayTransicionRepetida(E1, Etiqueta, Ls)).

esDeterministico(a(_,_,T)) :- transcionesDeterministicas(T).


% 2) estados(+Automata, ?Estados)
listaEstadosPorTransicion([], []).
listaEstadosPorTransicion([(S1, _, S2)|Ls], [S1,S2|Es]) :- listaEstadosPorTransicion(Ls, Es).

estados(a(I, F, T), Ls) :- setof(X, (listaEstadosPorTransicion(T, Y1), append(F, Y1, Y2), append([I], Y2, Y3), member(X, Y3)), Ls).


% 3)esCamino(+Automata, ?EstadoInicial, ?EstadoFinal, +Camino)


%hayTransicion(+T, +E1, +E2) Verdadero si hay una transicion
hayTransicion([(E1, _, E2)|_], E1, E2) :- !.
hayTransicion([(_, _, _)|Ls], E1, E2) :- hayTransicion(Ls, E1, E2).

%Si hay ciclos, no funciona la reversibilidad
esCamino(A, S, S, [S]) :- transicionesDe(A, T), hayTransicion(T, S, S).
esCamino(A, S, F, [S,L2|Ls]) :- transicionesDe(A, T), hayTransicion(T, S, L2), esCamino(A, L2, F, [L2|Ls]).
%esCamino(A, _, F, [L1,F]) :- transicionesDe(A, T), hayTransicion(T, L1, F), !.

% 4) ¿el predicado anterior es o no reversible con respecto a Camino y por qué?
% Responder aquí.
% No es reversible

% 5) caminoDeLongitud(+Automata, +N, -Camino, -Etiquetas, ?S1, ?S2)
etiquetaTransicion([(E1, Et, E2)|_], E1, E2, Et) :- !.
etiquetaTransicion([(_, _, _)|Ls], E1, E2, Et) :- etiquetaTransicion(Ls, E1, E2, Et).
etiquetaTransicion([], _, _, _) :- fail.

%Devuelve etiqueta y estado al que es posible moverse desde ese estado
%transicionesPosibles(+T, +E1, -E2, -Et)
transicionesPosibles([(E1, Et, E2)|_], E1, E2, Et).
transicionesPosibles([(_, _, _)|Ls], E1, E2, Et) :- transicionesPosibles(Ls, E1, E2, Et).

%Solo funciona para longitudes 1.
caminoDeLongitud(_, 1, [], [], _, _) :- !.
%Para casos pares
caminoDeLongitud(A, 2, [S,F], [E], S, F) :- transicionesDe(A, T), transicionesPosibles(T, S, F, E).
%Para casos impares, podría traer un problema el tema de indices
caminoDeLongitud(A, 1, [S, F], [E], S, F) :- transicionesDe(A, T), transicionesPosibles(T, S, F, E).
%Veo las maneras posibles de llegar
%Que pasa si CS es [] y N != 2
%Hay que decrementar de a 2, porque suponemos que S,C ya estan agregados a CS
%Esta todo medio emparchado pero parece andar
%Hay que decir que DEC >= para que no entre acá con 1 y entre en un loop infinito
%porque se pasaría N para los negativos y no tenemos un caso base para eso
caminoDeLongitud(A, N, [S,C|CS], [E|ES], S, F) :- transicionesDe(A, T), DEC is N-2, DEC >= 0, transicionesPosibles(T, S, C, E), caminoDeLongitud(A, DEC, [C|CS], ES, C, F).

% 6) alcanzable(+Automata, +Estado)
%Posible solucion
alcanzable(A, E) :- inicialDe(A, I), estados(A, X), length(X, N), between(2, N, Y), caminoDeLongitud(A, Y, _, _, I, E), !.

% 7) automataValido(+Automata)
automataValido(_).

%--- NOTA: De acá en adelante se asume que los autómatas son válidos.


% 8) hayCiclo(+Automata)
%Corto porque al encontrar un camino que sale y entra del mismo nodo, ya hay un ciclo.
hayCiclo(A) :- estados(A, E), length(E, LE), member(J, E), LB is LE+1, between(2, LB, LC), caminoDeLongitud(A, LC, _, _, J, J), !.

% 9) reconoce(+Automata, ?Palabra)
reconoce(_, _).

% 10) PalabraMásCorta(+Automata, ?Palabra)
palabraMasCorta(_, _).

%-----------------
%----- Tests -----
%-----------------

% Algunos tests de ejemplo. Deben agregar los suyos.

test(1) :- forall(ejemplo(_, A),  automataValido(A)).
test(2) :- not((ejemploMalo(_, A),  automataValido(A))).
test(3) :- ejemplo(10, A), reconoce(A, [p, X, r, X, d, i, _, m, X, s]).
test(4) :- ejemplo(9, A), reconoce(A, [a,  b,  a,  b,  a,  b,  a,  b]).
test(5) :- ejemplo(7, A), reconoce(A, [a,  a,  a,  b,  b]).
test(6) :- ejemplo(7, A), not(reconoce(A, [b])).
test(7) :- ejemplo(2, A),  findall(P, palabraMasCorta(A, P), [[]]).
test(8) :- ejemplo(4, A),  findall(P, palabraMasCorta(A, P), Lista), length(Lista, 2), sort(Lista, [[a], [b]]).
test(9) :- ejemplo(5, A),  findall(P, palabraMasCorta(A, P), Lista), length(Lista, 2), sort(Lista, [[b], [c]]).
test(10) :- ejemplo(6, A),  findall(P, palabraMasCorta(A, P), [[b, a]]).
test(11) :- ejemplo(7, A),  findall(P, palabraMasCorta(A, P), [[a, b]]).
test(12) :- ejemplo(8, A),  findall(P, palabraMasCorta(A, P), Lista), length(Lista, 2), sort(Lista, [[a,  a,  b,  f], [a,  b,  b,  f]]).
test(13) :- ejemplo(10, A),  findall(P, palabraMasCorta(A, P), [[p, r, o, l, o, g]]).
test(14) :- forall(member(X, [2, 4, 5, 6, 7, 8, 9]), (ejemplo(X, A), hayCiclo(A))).
test(15) :- not((member(X, [1, 3, 10]), ejemplo(X, A), hayCiclo(A))).
tests :- forall(between(1, 15, N), test(N)). %IMPORTANTE: Actualizar la cantidad total de tests para contemplar los que agreguen ustedes.
