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
%transcionesSonDeterministicas(+Transiciones)
transcionesSonDeterministicas([]).
transcionesSonDeterministicas([(E1, Etiqueta, _)|Ls]) :- forall(member(L, Ls), L \= (E1, Etiqueta, _)), transcionesDeterministicas(Ls).

esDeterministico(A) :- transicionesDe(A, T), transcionesDeterministicas(T).


% 2) estados(+Automata, ?Estados)
%listaEstadosPorTransicion(+Automata, ?Estados)
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
alcanzable(A, E) :- inicialDe(A, I), estados(A, X), length(X, N), CM = 2*N, between(2, CM, Y), caminoDeLongitud(A, Y, _, _, I, E), !.

% 7) automataValido(+Automata)
noTieneRepetidos([T1|TS]) :- forall(member(T2, TS), T1 \= T2), noTieneRepetidos(TS).

todosLosEstadosAlcanzablesDesdeInicial(A) :- estados(A, ES), forall(member(E, ES), alcanzable(A, E)).
tieneAlgunEstadoFinal(A) :- finalesDe(A, F), length(F, X), X >= 1.
noTieneTransicionesRepetidas(A) :- transicionesDe(A, T), noTieneRepetidos(T).
noTieneFinalesRepetidos(A) :- finales(A, F), noTieneRepetidos(F).

%Falta que todos los estados tengan salientes
automataValido(A) :- todosLosEstadosAlcanzablesDesdeInicial(A),
tieneAlgunEstadoFinal(A),
noTieneTransicionesRepetidas(A),
noTieneFinalesRepetidos(A).

%--- NOTA: De acá en adelante se asume que los autómatas son válidos.


% 8) hayCiclo(+Automata)
%Corto porque al encontrar un camino que sale y entra del mismo nodo, ya hay un ciclo.
hayCiclo(A) :- estados(A, E), length(E, LE), member(J, E), LB is LE+1, between(2, LB, LC), caminoDeLongitud(A, LC, _, _, J, J), !.

% 9) reconoce(+Automata, ?Palabra)
%Problema, como identificar que pare al no encontrar mas caminos
%El redo no es solamente por regla no? es por llamada a funcion

%Reconoce para listas de 1 elemento
%Re checkar esto
reconoce(A, [P]) :- inicialDe(A,I), finalesDe(A,F), member(I, F), P = I.
%Si no hay ciclo, busco entre todos los caminos posibles que empiecen en un inicial y terminen en un final
%Y unifiquen con P
%Uso CM = 2 * cantidad de estados, porque para transicionar a un estado se necesita un camino de al menos 2
reconoce(A, P) :- not(hayCiclo(A)), !, inicialDe(A,I), finalesDe(A,F),
	estados(A, E), length(E, LE), CM is 2*LE, between(1, CM, LC), member(Fm, F), caminoDeLongitud(A, LC, _, P, I, Fm), !.

%Separo en los casos de P definida y P no definida porque en el caso de que este definida, encuentra un valor y debería parar,
%no puede seguir generando otros valores porque P como está definido deja de unificar.
%En cambio, si no tengo P definida, debería dar todos los posibles.

%Si hay un ciclo y está definido P, encuentra 1 solucion y devuelve que lo reconocio
reconoce(A, P) :- hayCiclo(A), ground(P), inicialDe(A, I), finalesDe(A, F),
	desde(1, Y), member(Fm, F), caminoDeLongitud(A, Y, _, P, I, Fm), !.

%Si hay un ciclo y no está definido P, devuelvo todas las soluciones que pueda reonocer
reconoce(A, P) :- hayCiclo(A), not(ground(P)), inicialDe(A, I), finalesDe(A, F),
	desde(1, Y), member(Fm, F), caminoDeLongitud(A, Y, _, P, I, Fm).

% 10) PalabraMásCorta(+Automata, ?Palabra)
%La primera vez que reconozca una palabra, tira los resultados posibles para esa palabra
palabraMasCorta(A, P) :- desde(1, Y), length(P, Y), reconoce(A, P), !, reconoce(A,P), length(P, Y).

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
