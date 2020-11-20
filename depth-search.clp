(clear)

;
;      primera version del programa coto de caza
;      búsqueda primero en profundidad
;         

; Variables globales

(defglobal ?*filas* = 3)
(defglobal ?*columnas* = 3)

; **********************************************************************************************
; * Plantillas                                                                     
; **********************************************************************************************

(deftemplate estado
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
)

(deftemplate aumentar-explotacion
    (slot indice (type INTEGER) (range 1 9) )
    (slot estado (type SYMBOL) (allowed-symbols explotado pendiente))
)


;**************************************************************
;
;       regla inicial
;
;**************************************************************

(defrule inicial
      ?x <- (initial-fact)
=>
      (assert (estado (territorio N N N N) (padre sin-padre) (nodo 0) (nivel 1) (indice 1)))
      (assert (nodoactual 1))
      (retract ?x)
)


(defrule aumentar-explotacion
      (declare (salience 500))
      ?hecho <- (estado (territorio $?t) (nivel ?nivel) (indice ?i))
=>
      ; (assert (estado (territorio ?t) (padre ?hecho) (nivel (+ ?nivel 1))))
      (printout t "Aumentamos explotacion en el indice " ?i "" crlf)
      (assert (aumentar-explotacion (indice ?i) (estado pendiente)))
)

(defrule aplicar-explotacion
    ?e <- (aumentar-explotacion (indice ?i) (estado pendiente))
    ?hecho <- (estado (territorio $?t) (nivel ?nivel) (indice ?i))
     =>
    (printout t "Aumentamos explotación de " ?i "" crlf)
    (printout t "Valor  " (nth$ ?i ?t) "" crlf)

    (modify ?e (estado explotado))
)

