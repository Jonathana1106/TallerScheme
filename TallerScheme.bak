;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Ejercicios Clase|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; Funciones Lenguajes

;;############ Factorial ###############

(define (factorial n)
  (cond ((equal? n 0) 1)
        ((equal? n 1) 1)
        (else (* n (factorial(- n 1))))))

;;######################################

;;############ Fibonacci ###############

(define (fibonacci n)
  (cond ((equal? n 0) 0)
        ((equal? n 1) 1)
        (else (+ (fibonacci(- n 1)) (fibonacci(- n 2))))))

;;######################################

;;############ Miembro #################

(define (miembro num lista)
  (cond ((null? lista) (display "No se encuentra en lista \n"))
        ((equal? num (car lista)) #t)
        (else (miembro num (cdr lista)))))

;;######################################

;;############ Eliminar ################

(define (eliminar num lista)
  (cond ((null? lista) #f)
        ((equal? num (car lista)) (cdr lista))
        (else (let ((listaA

;;######################################

;;############ Quicksort ###############

;;######################################