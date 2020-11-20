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
      (slot estado
            (type SYMBOL)
            (allowed-symbols explotado pendiente))
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
      (assert (estado (territorio N N N N) (padre sin-padre) (nodo 0) (nivel 1) (indice 1) (estado pendiente)))
      (assert (nodoactual 1))
      (retract ?x)
)


(defrule aumentar-explotacion
      (declare (salience 500))
      ?hecho <- (estado (territorio $?t) (nivel ?nivel) (indice ?i) (estado ?e))
      (test (eq ?e pendiente)) ; TODO A modo de prueba solo nivel 1
=>
      (printout t "Aumentamos explotacion en el indice " ?i "" crlf)
      (assert (aumentar-explotacion (indice ?i) (estado pendiente)))
      (assert (estado (territorio ?t) (padre ?hecho) (nivel (+ ?nivel 1)) (estado explotado))) ; TODO A modo de prueba ya explotado
)

(defrule aplicar-explotacion
    ?e <- (aumentar-explotacion (indice ?i) (estado pendiente))
    ?hecho <- (estado (territorio $?t) (nivel ?nivel) (indice ?i))
     =>
    (printout t "Aumentamos explotación de " ?i "" crlf)
    (printout t "Valor  " (nth$ ?i ?t) "" crlf)
    ; Insertar un elemento en posición
    (modify ?hecho (territorio (replace$ ?t ?i ?i A)) (estado explotado))
    (printout t "Nuevo territorio  " (replace$ ?t ?i ?i A) "" crlf)
    (modify ?e (estado explotado))
)

