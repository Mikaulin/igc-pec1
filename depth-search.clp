(clear)

;
;      primera version del programa coto de caza
;      búsqueda primero en profundidad
;         

; Variables globales

(defglobal ?*filas* = 3)
(defglobal ?*columnas* = 3)
(defglobal ?*maximo* = 9)

; **********************************************************************************************
; * Plantillas                                                                     
; **********************************************************************************************

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
            (allowed-symbols tratado colindantes pendiente))
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
      (assert (nodoactual 1))
      (retract ?x)
)


(defrule aumentar-explotacion
      (declare (salience 500))
      ?hecho <- (arbol (territorio $?t) (nivel ?nivel) (indice ?i) (estado ?e))
      (test (and (< ?nivel 2) (eq ?e tratado))) ; TODO Solo hasta nivel 1
=>
      (printout t "Aumentamos explotacion en el indice " ?i "" crlf)      
      (assert (arbol (territorio ?t) (padre ?hecho) (nivel (+ ?nivel 1)) (indice ?i) (estado pendiente))) 
)

(defrule aplicar-explotacion
    (declare (salience 1000))
    ?hecho <- (arbol (territorio $?t) (nivel ?nivel) (indice ?i) (estado ?e))
    (test (and (< ?nivel 2) (eq ?e pendiente))) ; TODO Solo hasta nivel 1
     =>
    (bind ?ne (nth$ ?i ?t))
    (printout t "Explotar indice " ?i " con nivel " ?ne "" crlf)
    (if (eq ?ne N) then
        (modify ?hecho (territorio (replace$ ?t ?i ?i B)) (estado colindantes))
    )
    (if (eq ?ne B) then
        (modify ?hecho (territorio (replace$ ?t ?i ?i A)) (estado colindantes))
    )
    (if (eq ?ne A) then
        ;(modify ?hecho (nivel-explotacion N) (tratado si))
    )
    (printout t "Hecho modificado " ?hecho "" crlf)
)


(defrule aplicar-explotacion-colindantes
    (declare (salience 1500))
    ?hecho <- (arbol (territorio $?t) (nivel ?nivel) (indice ?i) (estado ?e))
    (test (and (< ?nivel 2) (eq ?e colindantes))) ; TODO Solo hasta nivel 1
     =>
    (printout t "Colindantes del indice " ?i "" crlf)
    ; Izquierda
    (if (> (- ?i 1) 0) then
        (printout t "Modificamos izquierda " (- ?i 1) "" crlf)
    )
    ; Derecha
    (if (< (+ ?i 1) ?*maximo*) then
        (printout t "Modificamos derecha " (+ ?i 1) "" crlf)
    )
    ; Arriba
    (if (> (- ?i ?*columnas*) 0) then
        (printout t "Modificamos arriba " (- ?i ?*columnas*) "" crlf)
    )
    ; Abajo
    (if (< (+ ?i ?*columnas*) ?*maximo*) then
        (printout t "Modificamos abajo " (+ ?i ?*columnas*) "" crlf)
    )
    
)
