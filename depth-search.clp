(clear)

(set-strategy depth)

;
;      primera version del programa coto de caza
;      búsqueda primero en profundidad
;         

; Variables globales

(defglobal ?*filas* = 2)
(defglobal ?*columnas* = 2)
(defglobal ?*maximo* = 4)

; **********************************************************************************************
; * Plantillas                                                                     
; **********************************************************************************************

(deftemplate explotar-casilla
      (slot indice
            (type INTEGER))
      (slot nodo
            (type INTEGER)
            (default 0))
)

(deftemplate arbol
      (multislot territorio
            (type SYMBOL))
      (slot padre
            (type FACT-ADDRESS SYMBOL)
            (allowed-symbols sin-padre)
            (default sin-padre))
      (slot nodo
            (type INTEGER)
            (default 0))
      (slot nivel
            (type INTEGER))
      (slot indice
            (type INTEGER))
      (slot estado
            (type SYMBOL)
            (allowed-symbols tratado pendiente))
)

;**************************************************************
;
;       regla inicial
;
;**************************************************************

(defrule inicial
      ?x <- (initial-fact)
=>
      (assert (arbol (territorio N N N N) (padre sin-padre) (nodo 0) (nivel 0) (indice 1) (estado tratado)))
      (assert (nodoactual 0))
      (retract ?x)
)


(defrule siguiente-nodo
      (declare (salience 500))
      ?nodoactual  <- (nodoactual ?contador)
      ?hecho <- (arbol (territorio $?t) (nivel ?nivel) (indice ?i) (estado tratado) (nodo ?n))
      (not (explotar-casilla)) ;La idea es que no haya ninguna casilla pendiente
      (test (= ?contador ?n))
=>
      (bind ?siguientenodo (+ ?contador 1))
      (assert (nodoactual ?siguientenodo))
      (retract ?nodoactual)

      (printout t "Nodo " ?siguientenodo "" crlf)
      (assert (arbol (territorio ?t) (padre ?hecho) (nivel (+ ?nivel 1)) (nodo ?siguientenodo) (indice ?i) (estado pendiente))) 
)

(defrule explotar-casilla
    (declare (salience 1000))
    ?hecho <- (arbol (territorio $?t) (nivel ?nivel) (indice ?i) (estado ?e) (nodo ?n))
    (test (eq ?e pendiente))
     =>
    (bind ?ne (nth$ ?i ?t))
    (printout t "Explotar indice " ?i " con nivel " ?ne "" crlf)
    (assert (explotar-casilla (indice ?i) (nodo ?n)))
)


(defrule explotar-arriba
    (declare (salience 1500))
    ?hecho <- (arbol (territorio $?t) (nivel ?nivel) (indice ?i) (estado pendiente) (nodo ?n))
    (test (> (- ?i ?*columnas*) 0))
     =>
    (printout t "Modificamos arriba " (- ?i ?*columnas*) "" crlf)
    (bind ?ne (nth$ (- ?i ?*columnas*) ?t))
    (assert (explotar-casilla (indice (- ?i ?*columnas*)) (nodo ?n)))
)

(defrule explotar-derecha
    (declare (salience 1500))
    ?hecho <- (arbol (territorio $?t) (nivel ?nivel) (indice ?i) (estado pendiente) (nodo ?n))
    (test (< (+ ?i 1) ?*maximo*))
     =>
    (printout t "Modificamos der " (+ ?i 1) "" crlf)
    (bind ?ne (nth$ (+ ?i 1) ?t))
    (assert (explotar-casilla (indice (+ ?i 1)) (nodo ?n)))
)

(defrule explotar-izquierda
    (declare (salience 1500))
    ?hecho <- (arbol (territorio $?t) (nivel ?nivel) (indice ?i) (estado pendiente) (nodo ?n))
    (test (> (- ?i 1) 0))
     =>
    (printout t "Modificamos izquierda " (- ?i 1) "" crlf)    
    (bind ?ne (nth$ (- ?i 1) ?t))
    (assert (explotar-casilla (indice (- ?i 1)) (nodo ?n)))
)

(defrule explotar-abajo
    (declare (salience 1500))
    ?hecho <- (arbol (territorio $?t) (nivel ?nivel) (indice ?i) (estado pendiente) (nodo ?n))
    (test (< (+ ?i ?*columnas*) ?*maximo*))
     =>
    (printout t "Modificamos abajo " (+ ?i ?*columnas*) "" crlf)  
    (bind ?ne (nth$ (+ ?i ?*columnas*) ?t))
    (assert (explotar-casilla (indice (+ ?i ?*columnas*)) (nodo ?n)))
)


(defrule aplicar-explotacion
    (declare (salience 500))
    ?casilla <- (explotar-casilla (indice ?ci))
    ?hecho <- (arbol (territorio $?t) (nivel ?nivel) (indice ?i) (estado ?e))
     =>
    (bind ?ne (nth$ ?ci ?t))
    (if (eq ?ne N) then
        (modify ?hecho (territorio (replace$ ?t ?ci ?ci B)) (estado tratado))
    )
    (if (eq ?ne B) then
        (modify ?hecho (territorio (replace$ ?t ?ci ?ci A)) (estado tratado))
    )
    (if (eq ?ne A) then
        (modify ?hecho (estado tratado))
    )
    (printout t "Aplicar explotacion en el indice " ?ci " con nivel " ?ne "" crlf)      
    (retract ?casilla)
)