;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Simpson-Rule) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;;crea una lista de [x0, x1 , x2 , x3 ...xn] donde {x0...[(xn-1)+{xn)] }
(define funcion1 (lambda ( a b n )
                  { letrec (
                            ;;1 crea lista de x0 , x1, x2 ...xn 
                             [formula ( / (- b a ) n )]
                             
                             [listax  ( lambda  (form limite)  (if (zero?  limite  ) empty
                                                              (cons form  (listax (+ form formula ) (- limite 1 )))) )])
                     
                    ;; 1
               (cons 0 (listax formula n ))  }))

;;(write ( funcion1 1 5 12 ))(newline)
                             
;; crea lista de [ 1 4 2 4 2 4 ...4 1 ] donde n es el largo de la lista  
(define lista042 (lambda (lista) (
                                  letrec (
                                          [v1 4 ]
                                          [v2 2]
                                          [limite ( /  ( - (length lista ) 5)2)]
                                          [listapar ( lambda (punt ) (if (> punt limite  ) empty
                                                              (cons  v2 ( cons  v1 (listapar (+ punt 1 ) )))))])
                                  (cons 1 (append '(4) (listapar 0 ) '(1) )))))

;;(write ( lista042 ( funcion1  1 5 12 )))(newline)


;;evalua cada dato de la lista en la funcion .Como resultado se obtiene una lista evaluada
(define funcion_evaluada
   (lambda (fx lista)
     (if (null? lista) empty
         (cons (fx (car lista)) (funcion_evaluada fx (cdr lista))))))

                                                
;;(write ( funcion_evaluada  (lambda (x) x ) ( funcion1  1 5 12 )))(newline)


;; multiplica la lista evaluada por la lista de [1 4 2 4 2... 1] ... retorna lista multiplicada
(define  multiplica (lambda (lista1 lista2) ( if  (null? lista1 ) empty
                                  (cons (* (car lista1)(car lista2))
                                        (multiplica (cdr lista1) (cdr lista2))))))


;;(write (multiplica ( funcion_evaluada  (lambda (x) x ) ( funcion1  1 5 12 )) (lista042  ( funcion1  1 5 12 ) )))


;suma los valores  de la lista multiplicada ...retorna un valor
(define suma (lambda ( lista) ( if (null? lista ) 0
                                   (+ (car lista) (suma (cdr lista ))))))

;(write(suma (multiplica ( funcion_evaluada  (lambda (x) x ) ( funcion1  1 5 12 )) (lista042  ( funcion1  1 5 12 ) ))))


;funcion principal que resive el valor y lo multiplica por ( h /3 )...retorna resultado esperado :3
(define simpson-rule (lambda ( fx a b n ) (

                                           
          * (/ (-  b a) (* n 3) )  (suma ( multiplica  (funcion_evaluada fx (funcion1 a  b n )) (lista042 (funcion1 a b n)))))            
                         ;; (suma    (multiplica  (funcion_evaluada fx (funcion1 a b n )) (lista042 (funcion1 a b n)))
                                   ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(write (simpson-rule (lambda (x) x )  1 5 4 ))(newline)
(write (simpson-rule (lambda (x) (* x x x) )  0 1 10 ))(newline)