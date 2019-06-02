;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname TallerScheme) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; Funciones Lenguajes
;; Instituto Tecnológico de Costa Rica
;; Prof: Marco Rivera
;; Alumno: Jonathan Guzmán Araya

;;############ Factorial ###############

;; Función que determina el factorial de un número entero.

(define (factorial n) ;; Nombre de la función y número que se desea analizar.
  (cond ((equal? n 0) 1) ;; Condición básica de parada.
        ((equal? n 1) 1) ;; Condición básica.
        (else (* n (factorial(- n 1)))))) ;; Llamada recursiva de la función para
                                          ;; determinar el factorial del número predecesor al actual.

;;######################################

;;############ Fibonacci ###############

(define (fibonacci n) ;; Nombre de la función y número que se desea analizar.
  (cond ((equal? n 0) 0) ;; Condición básica de parada.
        ((equal? n 1) 1) ;; Condición básica.
        (else (+ (fibonacci(- n 1)) (fibonacci(- n 2)))))) ;; Llamada recursiva de la función para
                                                           ;; determinar el fibonacci del número predecesor
                                                           ;; al actual y el predecesor del predecesor.

;;######################################

;;############ Miembro #################

(define (miembro num lista) ;; Nombre de la función y argumentos que recibe.
  (cond ((null? lista) (display "No se encuentra en lista \n")) ;; Condición de validación de la lista.
        ((equal? num (car lista)) #t) ;; Valida si el número se encuentra en la primera posición de la lista.
        (else (miembro num (cdr lista))))) ;; Si no se cumple el paso anterior elimina el primer elemento de
                                           ;; la lista y se llama recursivamente con el resto de la lista.

;;######################################

;;############ Eliminar ################

(define (eliminar num lista) ;; Nombre de la función y argumentos que recibe.
  (cond ((null? lista) '()) ;; Validación de que la lista no sea nula.
        ((equal? num (car lista)) (eliminar num (cdr lista))) ;; Compara si el elemento de la lista es igual
                                                              ;; al primero, y en caso de serlo lo elimina y
                                                              ;; llama recursivamente para ver si hay más elementos
                                                              ;; iguales en el resto de la lista.
        (else (cons (car lista) (eliminar num (cdr lista)))))) ;; Si no se cumple lo anterior saca el primer elemento
                                                               ;; de la lista y empieza a construir una nueva, además
                                                               ;; llama recursivamente a la función para ver si encuen
                                                               ;; tra el elemento a eliminar en el resto de la lista.

;;######################################

;;############ Quicksort ###############

(define (quickSort lista) ;; Nombre de la función y argumento que recibe.
  (cond ((null? lista) '()) ;; Validación de que la lista a ordenar no sea nula.
        (else (append (quickSort(menores lista)) (list(car lista)) (quickSort(mayores lista)))))) ;; Si no es nula, realiza un un append de
                                                                                                  ;; ordenar los menores al pivot, el pivot
                                                                                                  ;; y ordenar los mayores al pivot.

;;######################################

;;############ Menores #################

(define (menores lista) ;; Nombre de la función y argumento que recibe.
  (cond ((null? lista) '()) ;; Validación de que la lista no sea nula.
        (else (menoresAux (car lista) (cdr lista) '())))) ;;

(define (menoresAux pivot lista resultado) ;; Nombre de la función auxiliar y argumento que recibe.
  (cond ((null? lista) resultado) ;; Condición de parada.
        ((> pivot (car lista)) (menoresAux pivot (cdr lista) (append resultado (list (car lista))))) ;; Compara si el pivot es mayor que el
                                                                                                     ;; primer elemento de la lista, de ser
                                                                                                     ;; así lo agrega a la lista solución.
        (else (menoresAux pivot (cdr lista) resultado)))) ;; Si no es así, elimina el primer elemento
                                                          ;; de la lista y se llama recursivamente con
                                                          ;; el resto de la lista para continuar el análisis.

;; (menores '(5 8 1 3 7 4 6 2))

;;############ Mayores ################

(define (mayores lista) ;; Nombre de la función y argumento que recibe.
  (cond ((null? lista) '()) ;; Validación de que la lista no sea nula.
        (else (mayoresAux (car lista) (cdr lista) '())))) ;; Llama a la función auxiliar y le pasa los parámetros
                                                          ;; que recibe la misma

(define (mayoresAux pivot lista resultado) ;; Nombre de la función auxiliar y argumento que recibe.
  (cond ((null? lista) resultado) ;; Condición de parada.
        ((< pivot (car lista)) (mayoresAux pivot (cdr lista) (append resultado(list (car lista))))) ;; Compara si el pivot es menor que el
                                                                                                    ;; primer elemento de la lista, de ser
                                                                                                    ;; así lo agrega a la lista solución.
        (else (mayoresAux pivot (cdr lista) resultado)))) ;; Si no es así, elimina el primer elemento
                                                          ;; de la lista y se llama recursivamente con
                                                          ;; el resto de la lista para continuar el análisis.

;; (mayores '(5 8 1 3 7 4 6 2))

;;######################################

;;############ Automovil ################

(define (automovil valores atributos)
  (cond ((null? valores) '())
        (else (cons (list (car atributos) (car valores)) (automovil (cdr valores) (cdr atributos))))))

;;######################################

;;############ Arbol Binario ################

(define (tree centro izq der) ;; Función que contruye un árbol y recibe como parámetros la raíz y sus hijos izquierdo y derecho.
  (cond ((and (null? izq) (null? der)) centro) ;; Si el hijo izquierdo es nulo y su hijo derecho también entonces solo tiene raiz.
        (else (list centro izq der)))) ;; Si no, aagrega la raíz y sus hijos.

;; Detecta si un árbol no es representado por una lista.
(define (atom? x)
  (not (list? x)))

(define (raiz arbol) ;; Nombre de la función y argumento que recibe.
  (cond ((atom? arbol) arbol) ;; Si es árbol es atómico (compuesto de un solo elemento), lo retorna.
        (else (car arbol)))) ;; Si no, aplica un car del árbol que es su raíz.

(define (hijoIzq subArbol) ;; Función que encuentra el hijo izquierdo de un árbol o sub árbol.
  (cond ((atom? subArbol) '()) ;; Si el subarbol es el mismo, retorna vacío, indicando que no tiene hijos.
        (else (cadr subArbol)))) ;; Si no, del resto de la lista, toma su primer elemento que será el hijo izquierdo.

(define (hijoDer subArbol) ;; Función que encuentra el hijo derecho de un árbol o sub árbol.
  (cond ((atom? subArbol) '()) ;; Si el subarbol es el mismo, retorna vacío, indicando que no tiene hijos.
        (else (caddr subArbol)))) ;; Si no, del resto de la lista, toma su último elemento que será el hijo derecho.



(define (mayor arbol) ;; Función que encuentra el elmento mayor de un árbol.
  (cond  ((null? arbol) #f) ;; Si el árbol es nulo, retorna falso.
         ((null? (hijoDer arbol)) (raiz arbol)) ;; Si no tiene hijo dereho, el mayor será la raíz.
         (else (mayor (hijoDer arbol))))) ;; Si no, busca el mayor en su hijo derecho.

(define (eliminarArbol elemento arbol) ;; Función que elimina un elemento de un árbol.
  (cond ((null? arbol) arbol) ;; Si el árbol es vacío, se retorna a sí mismo.
        ((< elemento (raiz arbol)) (tree (raiz arbol) (eliminarArbol elemento (hijoIzq arbol)) (hijoDer arbol))) ;; Si el elemento a eliminar es menor que la raíz,
                                                                                                                 ;; contruye un árbol conservando la raíz eliminando
                                                                                                                 ;; el elemento de hijo izquierdo y conservando el
                                                                                                                 ;; hijo derecho.
        ((> elemento (raiz arbol)) (tree (raiz arbol) (hijoIzq arbol) (eliminarArbol elemento (hijoDer arbol)))) ;; Si el elemento a eliminar es mayor que la raíz,
                                                                                                                 ;; contruye un árbol conservando la raíz y a su hijo
                                                                                                                 ;; izquierdo y eliminando el elemento del hijo derecho.
        ((and (null? (hijoIzq arbol)) (null? (hijoDer arbol))) '()) ;; Comprueba si tiene hijos derecho e izquierdo.
        ((null? (hijoIzq arbol)) (hijoDer arbol)) ;; Solo tiene hijo derecho.
        ((null? (hijoDer arbol)) (hijoIzq arbol)) ;; Solo tiene hijo izquierdo.
        (else (tree (mayor (hijoIzq arbol)) (eliminarArbol (mayor (hijoIzq arbol)) (hijoIzq arbol)) (hijoDer arbol)) ))) ;; Si el elemento a eliminar es la raíz, construye
                                                                                                                         ;; un árbol con el mayor del hijo izquierdo como
                                                                                                                         ;; nueva raíz, elimina ese elemento del hijo izquierdo
                                                                                                                         ;; para devolver un nuevo hijo izquierdo y por último
                                                                                                                         ;; agrega el hijo derecho.

;;######################################

;;############ Anchura primero ################

;; Inorden (hijoIzq, raíz, hijoDer)
(define (anchuraIn arbol) ;; Función que realiza un recorrido en anchura de un árbol por inorden.
  (cond ((null? arbol) '()) ;; Si el árbol es nulo, se retorna a sí mismo.
        (else (append (append (anchuraIn(hijoIzq arbol)) (list (raiz arbol))) (anchuraIn(hijoDer arbol)))))) ;; Si no, contruye una lista realizando un recorrido inorden
                                                                                                             ;; en el hijo izquierdo, agrega la raíz y realiza un recorrido
                                                                                                             ;; inorden del hijo derecho.
;;######################################

;;########################################## Casos de Prueba ##########################################################################

(display "Factorialde 6 es: ");
(factorial 6);

(display "Fibonacci de 6 es: ");
(fibonacci 6);

(display "Miembro 5 de (1 2 3 4 5 6 7 8 9 0) es: ");
(miembro 5 '(1 2 3 4 5 6 7 8 9 0))

(display "Eliminar 5 de (1 2 3 4 5 6 7 8 9 0) entonces el resultado es: ");
(eliminar 5 '(1 2 3 4 5 6 7 8 9 0))

(display "Quicksort de (5 8 1 3 7 4 6 2) entonces el resultado es: "); 
(quickSort '(5 8 1 3 7 4 6 2))

(display "Automovil (Hatchback Suzuki Forza1 Rojo si Manual) (Tipo Marca Modelo Color AC Transmisión) entonces el resultado es: ");
(automovil '(Hatchback Suzuki Forza1 Rojo si Manual) '(Tipo Marca Modelo Color AC Transmisión))

(display "EliminarArbol 10 de (10 (5 3 8) (15 14 18)) entonces el resultado es: ");
(eliminarArbol 10 '(10 (5 3 8) (15 14 18)))

(display "Inorden arbol de (10 (5 3 8) (15 14 18)) entonces el resultado es: ");
(anchuraIn '(10 (5 3 8) (15 14 18)))

;;#######################################################################################################################################