% Autómatas de ejemplo. Si agregan otros,  mejor.

ejemplo(1, a(s1, [sf], [(s1, a, sf)])).
ejemplo(2, a(si, [si], [(si, a, si)])).
ejemplo(3, a(si, [si], [])).
ejemplo(4, a(s1, [s2, s3], [(s1, a, s1), (s1, a, s2), (s1, b, s3)])).
ejemplo(5, a(s1, [s2, s3], [(s1, a, s1), (s1, b, s2), (s1, c, s3), (s2, c, s3)])).
ejemplo(6, a(s1, [s3], [(s1, b, s2), (s3, n, s2), (s2, a, s3)])).
ejemplo(7, a(s1, [s2], [(s1, a, s3), (s3, a, s3), (s3, b, s2), (s2, b, s2)])).
ejemplo(8, a(s1, [sf], [(s1, a, s2), (s2, a, s3), (s2, b, s3), (s3, a, s1),
                        (s3, b, s2),(s3, b, s4), (s4, f, sf)])). % No deterministico :)
ejemplo(9, a(s1, [s1], [(s1, a, s2), (s2, b, s1)])).
ejemplo(10, a(s1, [s10, s11], [(s2, a, s3), (s4, a, s5), (s9, a, s10),
                               (s5, d, s6), (s7, g, s8), (s15, g, s11),
                               (s6, i, s7), (s13, l, s14), (s8, m, s9),
                               (s12, o, s13), (s14, o, s15), (s1, p, s2),
                               (s3, r, s4), (s2, r, s12), (s10, s, s11)])).
ejemplo(11, a(s1, [s4], [(s1, a, s2), (s1, b, s3), (s2, b, s4),
                         (s3, b, s4), (s1, c, s4)])).

ejemploMalo(1, a(s1, [s2], [(s1, a, s1), (s1, b, s2),
                            (s2, b, s2), (s2, a, s3)])). %s3 es un estado sin salida.
ejemploMalo(2, a(s1, [sf], [(s1, a, s1), (sf, b, sf)])). %sf no es alcanzable.
ejemploMalo(3, a(s1, [s2, s3], [(s1, a, s3), (s1, b, s3)])). %s2 no es alcanzable.
ejemploMalo(4, a(s1, [s3], [(s1, a, s3), (s2, b, s3)])). %s2 no es alcanzable.
ejemploMalo(5, a(s1, [s3, s2, s3], [(s1, a, s2), (s2, b, s3)])). %Tiene un estado final repetido.
ejemploMalo(6, a(s1, [s3], [(s1, a, s2), (s2, b, s3), (s1, a, s2)])). %Tiene una transición repetida.
ejemploMalo(7, a(s1, [], [(s1, a, s2), (s2, b, s3)])). %No tiene estados finales.

% Proyectores
inicialDe(a(I, _, _), I).

finalesDe(a(_, F, _), F).

transicionesDe(a(_, _, T), T).

% Auxiliar dado en clase.
% desde(+X, -Y)
desde(X, X).
desde(X, Y):-desde(X, Z),  Y is Z + 1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 1) esDeterministico(+Automata)                                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% transcionesSonDeterministicas(+Transiciones)
transcionesSonDeterministicas([]).
transcionesSonDeterministicas([(S, E, _) | Ts]) :- 
     forall(member(T, Ts), T \= (S, E, _)),
     transcionesSonDeterministicas(Ts).

esDeterministico(A) :- transicionesDe(A, T), transcionesSonDeterministicas(T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 2) estados(+Automata, ?Estados)                                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Nota: por estado de transición se entiende estado que no es ni inicial ni final.

% estadosDeTransicion(+Transiciones, ?Estados)
% Unifica Estados con una lista de los estados que participan en Transiciones
% (lista de transiciones).
estadosDeTransicion([], []).
estadosDeTransicion([(S1, _, S2) | Ts], [S1, S2 | Ss]) :-
    estadosDeTransicion(Ts, Ss).

estados(A, Estados) :-
    inicialDe(A, Inicial),
    finalesDe(A, Finales),
    transicionesDe(A, T),
    estadosDeTransicion(T, EstadosDeTransicion),
    append([Inicial | Finales], EstadosDeTransicion, Estados_),
    setof(X, member(X, Estados_), Estados).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 3) esCamino(+Automata, ?EstadoInicial, ?EstadoFinal, +Camino)                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% hayTransicion(+A, +S1, +S2)
% Se verifica si el autómata A tiene una transición entre los estados S1 y S2.
hayTransicion(A, S1, S2) :- transicionesDe(A, T),
                            member((S1, _, S2), T).

% Caminos de longitud 1.
esCamino(A, S, S, [S]) :- hayTransicion(A, S, S).

% Caminos de longitud >= 2.
esCamino(A, S, F, [S, F]) :- hayTransicion(A, S, F).
esCamino(A, S1, Sn, [S1, S2 | Ss]) :- hayTransicion(A, S1, S2),
                                      esCamino(A, S2, Sn, [S2 | Ss]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 4) ¿el predicado anterior es o no reversible con respecto a Camino y por qué?%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% No es reversible.
%
% En el caso que el autómata tuviera ciclos, podría ocurrir lo siguiente:
% 
% 1. Se unifica Camino con un camino que posee un ciclo.
% 2. Se hace backtracking hasta el goal asociado al término
%    hayTransición(A, S1, S2) en el que terminaba el ciclo del camino anterior.
% 3. Se resatisface dicho goal con valores para S1 y S2 tales que se realiza
%    otra "vuelta" por el mismo ciclo.
% 4. El intérprete continúa igual que en el camino original hasta unificar
%    Camino con un camino que posee dos ciclos.
% 5. Se vuelve al paso 2.
%
% En cada iteración se le agrega una nueva vuelta por el mismo ciclo al camino
% con el que se unifica la variable Camino, y nunca se sale de esta rama de
% ejecución. Si hubieran otros caminos entre EstadoInicial y EstadoFinal a los
% que se pudiera llegar evitando el ciclo, el predicado nunca los alcanzaría.
%
% Luego esCamino/4 no es reversible, pues no siempre unifica Camino con todos
% los caminos posibles entre EstadoInicial y EstadoFinal.
%
% Ejemplo concreto:
%
%   ?- ejemplo(5, A), esCamino(A, s1, s3, Camino).
%   A = a(s1, [s2, s3], [ (s1, a, s1), (s1, b, s2), (s1, c, s3), (s2, c, s3)]),
%   Camino = [s1, s3] ;
%   A = a(s1, [s2, s3], [ (s1, a, s1), (s1, b, s2), (s1, c, s3), (s2, c, s3)]),
%   Camino = [s1, s1, s3] ;
%   A = a(s1, [s2, s3], [ (s1, a, s1), (s1, b, s2), (s1, c, s3), (s2, c, s3)]),
%   Camino = [s1, s1, s1, s3] ;
%   A = a(s1, [s2, s3], [ (s1, a, s1), (s1, b, s2), (s1, c, s3), (s2, c, s3)]),
%   Camino = [s1, s1, s1, s1, s3] ;
%   A = a(s1, [s2, s3], [ (s1, a, s1), (s1, b, s2), (s1, c, s3), (s2, c, s3)]),
%   Camino = [s1, s1, s1, s1, s1, s3] 
%   ...
%
% Observar que el predicado unifica Camino con caminos de la forma 
% [s1, ..., s1, s3] pero nunca con caminos de la forma [s1, ..., s1, s2, s3].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 5) caminoDeLongitud(+Automata, +N, -Camino, -Etiquetas, ?S1, ?S2)            %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% transicionesPosibles(+T, +S1, -S2, -E)
% Toma una lista de transiciones T y un estado S1 y unifica S2 y E con
% un estado S2 alcanzable desde S1 por medio de la etiqueta E.
transicionesPosibles(T, S1, S2, E) :- member((S1, E, S2), T).

caminoDeLongitud(A, 1, [S], [], S, S) :- estados(A, E), member(S, E).
caminoDeLongitud(A, N, [S1, S2 | Camino], [E | Etiquetas], S1, Sn) :- 
    N >= 2,                                                   
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
                    between(1, N, M),
                    caminoDeLongitud(A, M, _, _, I, E),
                    !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 7) automataValido(+Automata)                                                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% estadosNoFinales(+Automata, ?EstadosNoFinales)
estadosNoFinales(A, EstadosNoFinales) :- 
    estados(A, Estados),
    finalesDe(A, EstadosFinales),
    subtract(Estados, EstadosFinales, EstadosNoFinales).

% todoEstadoNoFinalTieneTransicionesSalientes(+Automata)
todoEstadoNoFinalTieneTransicionesSalientes(A) :- 
     estadosNoFinales(A, EstadosNoFinales),
     transicionesDe(A, T),
     forall(member(E, EstadosNoFinales),
            transicionesPosibles(T, E, _, _)).

% estadosNoIniciales(+Automata, ?EstadosNoIniciales)
estadosNoIniciales(A, EstadosNoIniciales) :- 
     estados(A, Estados),
     inicialDe(A, Inicial),
     subtract(Estados, [Inicial], EstadosNoIniciales).

% todoEstadoEsAlcanzableDesdeElInicial(+Automata)
todoEstadoEsAlcanzableDesdeElInicial(A) :-
      estadosNoIniciales(A, EstadosNoIniciales),
      forall(member(E, EstadosNoIniciales),
             alcanzable(A, E)).

% tieneEstadosFinales(+Automata)            
tieneEstadosFinales(A) :- finalesDe(A, Finales), Finales \= [].

% noTieneRepetidos(+Lista)
noTieneRepetidos([]).
noTieneRepetidos([X | XS]) :- forall(member(Y, XS), X \= Y), noTieneRepetidos(XS).

% noHayEstadosFinalesRepetidos(+Automata)
noHayEstadosFinalesRepetidos(A) :- finalesDe(A, F), noTieneRepetidos(F).

% noHayTransicionesRepetidas(+Automata)
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

% Se corta luego de encontrar el primer ciclo para evitar recorrer el resto del
% espacio de búsqueda innecesariamente.
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

% longitudCamino(+Automata, ?Palabra, -N)
% Unifica N con la longitud de Palabra en caso que estuviera instanciada o semi-
% instanciada (ej.: [h,X,l,a]), o genera las posibles longitudes de palabra en
% caso que no estuviera instanciada.
longitudCamino(_, Palabra, N) :- nonvar(Palabra), length(Palabra, M), N is M + 1.
longitudCamino(A, Palabra, N) :- var(Palabra), hayCiclo(A), desde(1, N).
longitudCamino(A, Palabra, N) :- var(Palabra), not(hayCiclo(A)),
                                 estados(A, Estados),
                                 length(Estados, NE),
                                 between(1, NE, N).

% reconoce(+Automata, ?Palabra)                                 
reconoce(A, Palabra) :- inicialDe(A, Inicial),
                        finalesDe(A, Finales),
                        longitudCamino(A, Palabra, N),
                        member(Final, Finales),
                        caminoDeLongitud(A, N, _, Palabra, Inicial, Final).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 10) PalabraMásCorta(+Automata, ?Palabra)                                     %  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% palabrasDeLongitudN(+Automata, ?Palabras, +N)
% Unifica Palabras con una lista de todas las palabras de longitud N aceptadas
% por el autómata.
palabrasDeLongitudN(A, Palabras, N) :-
        findall(P, (length(P, N), reconoce(A, P)), Palabras).

palabraMasCorta(A, P) :- desde(0, N),
                         palabrasDeLongitudN(A, Palabras, N),
                         Palabras \= [],
                         !,
                         member(P, Palabras).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
% Tests                                                                        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	

numeroDeTests(44).
tests :- numeroDeTests(N), forall(between(1, N, I), test(I)). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
% Tests provistos por la cátedra                                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	

test(1)  :- forall(ejemplo(_, A),  automataValido(A)).
test(2)  :- not((ejemploMalo(_, A),  automataValido(A))).
test(3)  :- ejemplo(10, A), reconoce(A, [p, X, r, X, d, i, _, m, X, s]).
test(4)  :- ejemplo(9, A), reconoce(A, [a,  b,  a,  b,  a,  b,  a,  b]).
test(5)  :- ejemplo(7, A), reconoce(A, [a,  a,  a,  b,  b]).
test(6)  :- ejemplo(7, A), not(reconoce(A, [b])).
test(7)  :- ejemplo(2, A),  findall(P, palabraMasCorta(A, P), [[]]).
test(8)  :- ejemplo(4, A),  findall(P, palabraMasCorta(A, P), Lista),
            length(Lista, 2), sort(Lista, [[a], [b]]).
test(9)  :- ejemplo(5, A),  findall(P, palabraMasCorta(A, P), Lista),
            length(Lista, 2), sort(Lista, [[b], [c]]).
test(10) :- ejemplo(6, A),  findall(P, palabraMasCorta(A, P), [[b, a]]).
test(11) :- ejemplo(7, A),  findall(P, palabraMasCorta(A, P), [[a, b]]).
test(12) :- ejemplo(8, A),  findall(P, palabraMasCorta(A, P), Lista),
            length(Lista, 2), sort(Lista, [[a,  a,  b,  f], [a,  b,  b,  f]]).
test(13) :- ejemplo(10, A),  findall(P, palabraMasCorta(A, P), [[p, r, o, l, o, g]]).
test(14) :- forall(member(X, [2, 4, 5, 6, 7, 8, 9]), (ejemplo(X, A), hayCiclo(A))).
test(15) :- not((member(X, [1, 3, 10]), ejemplo(X, A), hayCiclo(A))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tests propios                                                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Ejercicio 1 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Una sola transición, sin ciclos.
test(16) :- ejemplo(1, A), esDeterministico(A).
% Una sola transición, con ciclos.
test(17) :- ejemplo(2, A), esDeterministico(A).
% Sin transiciones.
test(18) :- ejemplo(3, A), esDeterministico(A).
% Varias transiciones, no determinístico.
test(19) :- ejemplo(4, A), not(esDeterministico(A)).
% Varias transiciones, determinístico.
test(20) :- ejemplo(5, A), esDeterministico(A).

% Ejercicio 2 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Nota: por estado de transición se entiende estado que no es ni inicial ni final.

% Sin estados de transición, con transiciones.
test(21) :- ejemplo(1, A), estados(A, Xs), permutation(Xs, [s1, sf]).
% Sin transiciones.
test(22) :- ejemplo(2, A), estados(A, Xs), Xs = [si].
% Un único estado (el inicial).
test(23) :- ejemplo(3, A), estados(A, Xs), Xs = [si].
% Más de un estado final, sin estados de transición.
test(24) :- ejemplo(4, A), estados(A, Xs), permutation(Xs, [s1, s2, s3]).
% Un estado final, un estado de transición.
test(25) :- ejemplo(6, A), estados(A, Xs), permutation(Xs, [s1, s2, s3]). 
% Más de un estado final, más de un estado de transición.
test(26) :- ejemplo(10, A), estados(A, Xs),
            permutation(Xs, [s1, s2, s3, s4, s5, s6, s7, s8,
                             s9, s10, s11, s12, s13, s14, s15]).

% Ejercicio 3 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Camino de longitud 0 (inválido).
test(27) :- ejemplo(1, A), not(esCamino(A, s1, s1, [])). 
% Camino de longitud 1 (ciclo a sí mismo).
test(28) :- ejemplo(2, A), esCamino(A, si, si, [si]). 
% Camino de longitud 2.
test(29) :- ejemplo(1, A), esCamino(A, s1, sf, [s1, sf]). 
% Camino de longitud 2 inválido (origen/destino invertidos, camino invertido).
test(30) :- ejemplo(1, A), not(esCamino(A, sf, s1, [sf, s1])). 
% Camino de longitud 2 inválido (origen/destino invertidos, camino correcto).
test(31) :- ejemplo(1, A), not(esCamino(A, sf, s1, [s1, sf])). 
% Camino de longitud 2 inválido (origen/destino correctos, camino invertido).
test(32) :- ejemplo(1, A), not(esCamino(A, s1, sf, [sf, s1])). 
% Camino de longitud 7 (origen/destino no instanciados - palabra "prolog").
test(33) :- ejemplo(10, A), esCamino(A, S1, S2, [s1, s2, s12, s13, s14, s15, s11]),
            S1 = s1, S2 = s11.
% Camino de longitud 7 inválido (origen/destino no instanciados, camino inválido).
test(34) :- ejemplo(10, A), not(esCamino(A, _, _, [s1, s2, s11, s12, s13, s14, s15])).
% Camino de longitud 7 inválido (origen/destino incorrectos - palabra "prolog").
test(35) :- ejemplo(10, A), not(esCamino(A, s2, s15, [s1, s2, s12, s13, s14, s15, s11])).

% Ejercicio 5 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Camino de longitud 1 (origen/destino instanciados).
test(36) :- ejemplo(1, A), caminoDeLongitud(A, 1, C, E, s1, s1), C = [s1], E = [].
% Caminos de longitud 1 (origen/destino no instanciados).
test(37) :- ejemplo(1, A), findall((C, E, S1, S2),
                                   caminoDeLongitud(A, 1, C, E, S1, S2),
                                   Xs),
            permutation(Xs, [([s1], [], s1, s1), ([sf], [], sf, sf)]).
% Camino de longitud 2 válido (origen/destino instanciados).
test(38) :- ejemplo(1, A), caminoDeLongitud(A, 2, C, E, s1, sf), C = [s1, sf], E = [a]. 
% Camino de longitud 2 válido (origen/destino no instanciados).
test(39) :- ejemplo(1, A), caminoDeLongitud(A, 2, C, E, S1, S2),
            C = [s1, sf], E = [a], S1 = s1, S2 = sf.
% Camino de longitud 2 inválido (origen/destino invertidos).
test(40) :- ejemplo(1, A), not(caminoDeLongitud(A, 2, _, _, sf, s1)).
% Caminos de longitud 3 (origen/destino instanciados).
test(41) :- ejemplo(5, A), findall((C, E), caminoDeLongitud(A, 3, C, E, s1, s3), Xs),
            permutation(Xs, [([s1, s1, s3], [a, c]), ([s1, s2, s3], [b, c])]).
% Caminos de longitud 3 (origen/destino no instanciados).
test(42) :- ejemplo(5, A), findall((C, E, S1, S2),
                                   caminoDeLongitud(A, 3, C, E, S1, S2),
                                   Xs),
            permutation(Xs, [([s1, s1, s1], [a, a], s1, s1),
                             ([s1, s1, s2], [a, b], s1, s2),
                             ([s1, s1, s3], [a, c], s1, s3),
                             ([s1, s2, s3], [b, c], s1, s3)]).

% Ejercicio 6 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Todos los estados de todos los autómatas de ejemplo son alcanzables.
test(43) :- forall(ejemplo(_, A), (estados(A, Es), forall(member(E, Es),
                                                          alcanzable(A, E)))).
% Los autómatas de ejemplos malos con estados no alcanzables efectivamente tienen
% estados no alcanzables.
test(44) :- forall(between(2, 4, N), (ejemploMalo(N, A),
                                      estados(A, Es),
                                      member(E, Es),
                                      not(alcanzable(A, E)))).

% Ejercicio 7 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Cubierto por los tests provistos por la cátedra.

% Ejercicio 8 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Cubierto por los tests provistos por la cátedra.

% Ejercicio 9 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Cubierto por los tests provistos por la cátedra.

% Ejercicio 10 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Cubierto por los tests provistos por la cátedra.
