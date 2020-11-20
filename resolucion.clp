(clear)

; Variables globales

(defglobal ?*filas* = 3)
(defglobal ?*columnas* = 3)

; **********************************************************************************************
; * Plantillas                                                                     
; **********************************************************************************************

(deftemplate aumentar-explotacion
    (slot fila (type INTEGER) (range 1 3))
    (slot columna (type INTEGER) (range 1 3))
    (slot estado (type SYMBOL) (allowed-symbols explotado pendiente))
)


(deftemplate cuadricula
    (slot fila (type INTEGER) (range 1 3))
    (slot columna (type INTEGER) (range 1 3))
    (slot nivel-explotacion (type SYMBOL) (allowed-symbols N B A))
    (slot tratado (type SYMBOL) (allowed-symbols si no))
)

; **********************************************************************************************
; * Hechos                                                                     
; **********************************************************************************************

(deffacts territorio
    (cuadricula (fila 1) (columna 1) (nivel-explotacion N) (tratado no))
    (cuadricula (fila 1) (columna 2) (nivel-explotacion N) (tratado no))
    (cuadricula (fila 1) (columna 3) (nivel-explotacion N) (tratado no))
    (cuadricula (fila 2) (columna 1) (nivel-explotacion N) (tratado no))
    (cuadricula (fila 2) (columna 2) (nivel-explotacion N) (tratado no))
    (cuadricula (fila 2) (columna 3) (nivel-explotacion N) (tratado no))
    (cuadricula (fila 3) (columna 1) (nivel-explotacion N) (tratado no))
    (cuadricula (fila 3) (columna 2) (nivel-explotacion N) (tratado no))
    (cuadricula (fila 3) (columna 3) (nivel-explotacion N) (tratado no))
)


; **********************************************************************************************
; * Reglas                                                                     
; **********************************************************************************************

(defrule pide-valor
    (initial-fact)
     =>
    (printout t "Introduzca cuadrila donde aumentar la explotacion. " crlf)
    (printout t "Fila: " crlf)
	(bind ?fila (read))
	(printout t "Columna: " crlf)
	(bind ?columna (read))
    (assert (aumentar-explotacion (fila ?fila) (columna ?columna) (estado pendiente)))
)

(defrule aplicar-explotacion
    ?e <- (aumentar-explotacion (fila ?x) (columna ?y) (estado pendiente))
    ?c <- (cuadricula (fila ?x) (columna ?y) (nivel-explotacion ?ne) (tratado no))
     =>
    (printout t "Aumentamos explotación de " ?x ", " ?y "" crlf)
    (if (eq ?ne N) then
        (modify ?c (nivel-explotacion B) (tratado si))
    )
    (if (eq ?ne B) then
        (modify ?c (nivel-explotacion A) (tratado si))
    )
    (if (eq ?ne A) then
        (modify ?c (nivel-explotacion N) (tratado si))
    )
    (modify ?e (estado explotado))
)

(defrule aplicar-explotacion-colindantes
    ?e <- (aumentar-explotacion (fila ?x) (columna ?y) (estado explotado))
     =>
    ; Quitamos el origen de la explotación ?
    (retract ?e)
    ; Cuadrículas en la misma columna
    (if (> ?x 1) then
        (printout t "Hecho nuevo para aumentar en " (- ?x 1) ", " ?y "" crlf)
        (assert (aumentar-explotacion (fila (- ?x 1)) (columna ?y) (estado pendiente)))
        
    )
    (if (< ?x 3) then
        (printout t "Hecho nuevo para aumentar en " (+ ?x 1) ", " ?y "" crlf)
        (assert (aumentar-explotacion (fila (+ ?x 1)) (columna ?y) (estado pendiente)))
    )
    ; Cuadrículas en la misma fila
    (if (> ?y 1) then
        (printout t "Hecho nuevo para aumentar en " ?x ", " (- ?y 1) "" crlf)
        (assert (aumentar-explotacion (fila ?x) (columna (- ?y 1)) (estado pendiente)))
        
    )
    (if (< ?y 3) then
        (printout t "Hecho nuevo para aumentar en " ?x ", " (+ ?y 1) "" crlf)
        (assert (aumentar-explotacion (fila ?x) (columna (+ ?y 1)) (estado pendiente)))
    )   
)

(defrule eliminar-aplicacion-explotacion
    ?e <- (aumentar-explotacion (fila ?x) (columna ?y) (estado pendiente))
    ?c <- (cuadricula (fila ?x) (columna ?y) (nivel-explotacion ?ne) (tratado si))
     =>
    (retract ?e)
)
