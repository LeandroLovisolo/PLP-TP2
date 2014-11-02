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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 1) %esDeterministico(+Automata)                                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%transcionesSonDeterministicas(+Transiciones)
transcionesSonDeterministicas([]).
transcionesSonDeterministicas([(E1, Etiqueta, _)|Ls]) :- forall(member(L, Ls),
                                                                L \= (E1, Etiqueta, _)),
                                                         transcionesSonDeterministicas(Ls).

esDeterministico(A) :- transicionesDe(A, T), transcionesSonDeterministicas(T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 2) estados(+Automata, ?Estados)                                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%listaEstadosPorTransicion(+Automata, ?Estados)
listaEstadosPorTransicion([], []).
listaEstadosPorTransicion([(S1, _, S2)|Ls], [S1,S2|Es]) :- listaEstadosPorTransicion(Ls, Es).

estados(a(I, F, T), Ls) :- setof(X, (listaEstadosPorTransicion(T, Y1), append(F, Y1, Y2), append([I], Y2, Y3), member(X, Y3)), Ls).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 3)esCamino(+Automata, ?EstadoInicial, ?EstadoFinal, +Camino)                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%hayTransicion(+T, +E1, +E2) Verdadero si hay una transicion
hayTransicion(T, E1, E2) :- member((E1, _, E2), T).

%Si hay ciclos, no funciona la reversibilidad
esCamino(A, S, S, [S]) :- transicionesDe(A, T), hayTransicion(T, S, S).
esCamino(A, S, F, [S,F]) :- transicionesDe(A, T), hayTransicion(T, S, F), !.
esCamino(A, S, F, [S,L2|Ls]) :- transicionesDe(A, T), hayTransicion(T, S, L2), esCamino(A, L2, F, [L2|Ls]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 4) ¿el predicado anterior es o no reversible con respecto a Camino y por qué?%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Responder aquí.
% No es reversible

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 5) caminoDeLongitud(+Automata, +N, -Camino, -Etiquetas, ?S1, ?S2)            %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Devuelve etiqueta y estado al que es posible moverse desde ese estado
%transicionesPosibles(+T, +E1, -E2, -Et)
transicionesPosibles(T, S1, S2, E) :- member((S1, E, S2), T).

caminoDeLongitud(A, 1, [S], [], S, S) :- estados(A, E), member(S, E).
caminoDeLongitud(A, N, [S1, S2 | Camino], [E | Etiquetas], S1, Sn) :- N >= 2,
                                                                      transicionesDe(A, T),
                                                                      transicionesPosibles(T, S1, S2, E),
                                                                      M is N - 1,
                                                                      caminoDeLongitud(A, M, [S2 | Camino], Etiquetas, S2, Sn).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 6) alcanzable(+Automata, +Estado)                                            %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

alcanzable(A, E) :- inicialDe(A, I),
                    estados(A, Estados),
                    length(Estados, N),
                    between(2, N, M),
                    caminoDeLongitud(A, M, _, _, I, E),
                    !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 7) automataValido(+Automata)                                                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

estadosNoFinales(A, EstadosNoFinales) :- estados(A, Estados),
                                         finalesDe(A, EstadosFinales),
                                         subtract(Estados, EstadosFinales, EstadosNoFinales).

todoEstadoNoFinalTieneTransicionesSalientes(A) :- estadosNoFinales(A, EstadosNoFinales),
                                                  transicionesDe(A, T),                     
                                                  forall(member(E, EstadosNoFinales),
                                                         transicionesPosibles(T, E, _, _)).

estadosNoIniciales(A, EstadosNoIniciales) :- estados(A, Estados),
                                             inicialDe(A, Inicial),
                                             subtract(Estados, [Inicial], EstadosNoIniciales).

todoEstadoEsAlcanzableDesdeElInicial(A) :- estadosNoIniciales(A, EstadosNoIniciales),
                                           forall(member(E, EstadosNoIniciales),
                                                  alcanzable(A, E)).
tieneEstadosFinales(A) :- finalesDe(A, Finales), Finales \= [].

noTieneRepetidos([]).
noTieneRepetidos([X | XS]) :- forall(member(Y, XS), X \= Y), noTieneRepetidos(XS).

noHayEstadosFinalesRepetidos(A) :- finalesDe(A, F), noTieneRepetidos(F).

noHayTransicionesRepetidas(A) :- transicionesDe(A, T), noTieneRepetidos(T).

automataValido(A) :- todoEstadoNoFinalTieneTransicionesSalientes(A),
                     todoEstadoEsAlcanzableDesdeElInicial(A),
                     tieneEstadosFinales(A),
                     noHayEstadosFinalesRepetidos(A),
                     noHayTransicionesRepetidas(A).

%--- NOTA: De acá en adelante se asume que los autómatas son válidos.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 8) hayCiclo(+Automata)                                                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Corto porque al encontrar un camino que sale y entra del mismo nodo, ya hay un ciclo.
hayCiclo(A) :- estados(A, Estados),
               length(Estados, N),
               member(E, Estados),
               M is N + 1,
               between(2, M, K),
               caminoDeLongitud(A, K, _, _, E, E),
               !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 9) reconoce(+Automata, ?Palabra)                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

longitudCamino(_, Palabra, N) :- nonvar(Palabra), length(Palabra, M), N is M + 1.
longitudCamino(A, Palabra, N) :- var(Palabra), hayCiclo(A), desde(1, N).
longitudCamino(A, Palabra, N) :- var(Palabra), not(hayCiclo(A)),
                                 estados(A, Estados),
                                 length(Estados, NE),
                                 between(1, NE, N).
reconoceUna(A, Palabra) :- inicialDe(A, Inicial),
                           finalesDe(A, Finales),
                           longitudCamino(A, Palabra, N),
                           member(Final, Finales),
                           caminoDeLongitud(A, N, _, Palabra, Inicial, Final).
reconoce(A, Palabra) :- ground(Palabra), reconoceUna(A, Palabra), !.
reconoce(A, Palabra) :- not(ground(Palabra)), reconoceUna(A, Palabra).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 10) PalabraMásCorta(+Automata, ?Palabra)                                     %  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%La primera vez que reconozca una palabra, tira los resultados posibles para esa palabra
%Parece que el redo hace las cosas 4 veces. Hay resultados repetidos
palabraMasCorta(A, P) :- desde(0, Y),
                         length(P, Y),
                         length(X, Y),
                         reconoce(A, X),
                         !,
                         reconoce(A, P).

palabraMasCorta(A, P) :- desde(0, N),
                         palabrasDeLongitudN(A, Palabras, N),
                         Palabras \= [],
                         !,
                         member(P, Palabras).
palabrasDeLongitudN(A, Palabras, N) :-
        findall(P, (length(P, N), reconoce(A, P)), Palabras).

%-----------------
%----- Tests -----
%-----------------

numeroDeTests(30).
tests :- numeroDeTests(N), forall(between(1, N, I), test(I)). 

% Tests provistos por la cátedra. 

test(1)  :- forall(ejemplo(_, A),  automataValido(A)).
test(2)  :- not((ejemploMalo(_, A),  automataValido(A))).
test(3)  :- ejemplo(10, A), reconoce(A, [p, X, r, X, d, i, _, m, X, s]).
test(4)  :- ejemplo(9, A), reconoce(A, [a,  b,  a,  b,  a,  b,  a,  b]).
test(5)  :- ejemplo(7, A), reconoce(A, [a,  a,  a,  b,  b]).
test(6)  :- ejemplo(7, A), not(reconoce(A, [b])).
test(7)  :- ejemplo(2, A),  findall(P, palabraMasCorta(A, P), [[]]).
test(8)  :- ejemplo(4, A),  findall(P, palabraMasCorta(A, P), Lista), length(Lista, 2), sort(Lista, [[a], [b]]).
test(9)  :- ejemplo(5, A),  findall(P, palabraMasCorta(A, P), Lista), length(Lista, 2), sort(Lista, [[b], [c]]).
test(10) :- ejemplo(6, A),  findall(P, palabraMasCorta(A, P), [[b, a]]).
test(11) :- ejemplo(7, A),  findall(P, palabraMasCorta(A, P), [[a, b]]).
test(12) :- ejemplo(8, A),  findall(P, palabraMasCorta(A, P), Lista), length(Lista, 2), sort(Lista, [[a,  a,  b,  f], [a,  b,  b,  f]]).
test(13) :- ejemplo(10, A),  findall(P, palabraMasCorta(A, P), [[p, r, o, l, o, g]]).
test(14) :- forall(member(X, [2, 4, 5, 6, 7, 8, 9]), (ejemplo(X, A), hayCiclo(A))).
test(15) :- not((member(X, [1, 3, 10]), ejemplo(X, A), hayCiclo(A))).

% Tests propios.

% Ejercicio 1
test(16) :- ejemplo(1, A), esDeterministico(A).
test(17) :- ejemplo(2, A), esDeterministico(A).
test(18) :- ejemplo(3, A), esDeterministico(A).
test(19) :- ejemplo(4, A), not(esDeterministico(A)).
test(20) :- ejemplo(5, A), esDeterministico(A).

% Ejercicio 2
test(21) :- ejemplo(1, A), estados(A, [s1, sf]). 
test(22) :- ejemplo(2, A), estados(A, [si]). 
test(23) :- ejemplo(3, A), estados(A, [si]). 
test(24) :- ejemplo(4, A), estados(A, [s1, s2, s3]). 
test(25) :- ejemplo(5, A), estados(A, [s1, s2, s3]). 

%Ejercicio 3
test(26) :- ejemplo(4, A), esCamino(A, s1, s2, [s1,s2]).
test(27) :- ejemplo(5, A), esCamino(A, S, F, [s1, s2, s3]), S = s1, F = s3.
test(28) :- ejemplo(10, A), not(esCamino(A, s6, s15, [s6,s7,s8,s9,s15])).
test(29) :- ejemplo(10, A), esCamino(A, S, F, [s14, s15, s11]), S = s14, F = s11.
test(30) :- ejemploMalo(2, A), not(esCamino(A, s1, sf, [s1, sf])).
