(clear)

(defglobal ?*filas* = 2)
(defglobal ?*columnas* = 2)
(defglobal ?*maximo* = 4)

;**************************************************************
;
;       (set-strategy breadth) / (set-strategy depth)
;
;       Aquí se ve cómo se genera el árbol en profundidad y anchura
;       Ahora quedará obtener índice del multislot y explotar colindantes
;       Y después aplicar podas, eliminar duplicados...
;       Ejemplo el Ejercicio24.clp
;**************************************************************

(deftemplate casilla-explotada
      (slot indice
            (type INTEGER))
      (slot nodo
            (type INTEGER)
            (default 0))
)

(deftemplate colindante
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
)


(defrule inicial
      ?x <- (initial-fact)
=>
      (assert (arbol (territorio A N N N) (padre sin-padre) (nodo 0) (nivel 0) (indice 1)))
      (assert (nodoactual 0))
      (retract ?x)
)

(defrule explotar-N-B
      ?hecho <- (arbol (territorio $?primeras ?N $?ultimas) (nivel ?nivel) (indice ?i) (nodo ?n))
      (test (and (eq ?N N) (< ?nivel 3)))
=>
      (bind ?posicion (member$ ?N (create$ $?primeras ?N $?ultimas)))
      (printout t "Posicion " ?posicion "" crlf)
      (printout t "Nuevo territorio " $?primeras B $?ultimas "" crlf)
      (assert (arbol (territorio $?primeras B $?ultimas) (padre ?hecho) (nivel (+ ?nivel 1)) (indice ?posicion)))  
      
      ; Nuevo
      (assert (casilla-explotada (indice (member$ ?N (create$ $?primeras ?N $?ultimas)))))
)

(defrule explotar-B-A
      ?hecho <- (arbol (territorio $?primeras ?B $?ultimas) (nivel ?nivel) (indice ?i) (nodo ?n))
      (test (and (eq ?B B) (< ?nivel 3)))
=>
      (bind ?posicion (member$ ?B (create$ $?primeras ?B $?ultimas)))
      (printout t "Posicion " ?posicion "" crlf)
      (printout t "Nuevo territorio " $?primeras A $?ultimas "" crlf)
      (assert (arbol (territorio $?primeras A $?ultimas) (padre ?hecho) (nivel (+ ?nivel 1)) (indice ?posicion)))  
      
      ; Nuevo
      (assert (casilla-explotada (indice (member$ ?B (create$ $?primeras ?B $?ultimas)))))
)

(defrule contador-nodos      
      (declare (salience 1000))
      ?arbol <- (arbol (nodo ?nodoestado))
      ?hechonodo <- (nodoactual ?contador)
      (test(= ?nodoestado 0))
=>
      (assert (nodoactual (+ 1 ?contador)))
      (modify ?arbol (nodo ?contador))
      (retract ?hechonodo)
)

(defrule nodo-casilla-explotada
      (declare (salience 1000))
      ?explotada <- (casilla-explotada (indice ?indice) (nodo ?nodo))
      ?hechonodo <- (nodoactual ?contador)
      (test(= ?nodo 0))
=>
      (modify ?explotada (nodo ?contador))
)


(defrule explotar-arriba
      (declare (salience 950))
      ?arbol <- (arbol (territorio $?t) (nodo ?nodoestado))
      ?explotada <- (casilla-explotada (indice ?indice) (nodo ?nodo))
      (test (and (neq ?nodo 0) (= ?nodo ?nodoestado) (> (- ?indice ?*columnas*) 0)))

=>
      (printout t "Modificamos arriba " (- ?indice ?*columnas*) "" crlf)
      (assert (colindante (indice (- ?indice ?*columnas*)) (nodo ?nodo)))
)

(defrule explotar-abajo
      (declare (salience 950))
      ?arbol <- (arbol (territorio $?t) (nodo ?nodoestado))
      ?explotada <- (casilla-explotada (indice ?indice) (nodo ?nodo))
      (test (and (neq ?nodo 0) (= ?nodo ?nodoestado) (< (+ ?indice ?*columnas*) ?*maximo*)))

=>
      (printout t "Modificamos abajo " (+ ?indice ?*columnas*) "" crlf)
      (assert (colindante (indice (+ ?indice ?*columnas*)) (nodo ?nodo)))
)

(defrule explotar-derecha
      (declare (salience 950))
      ?arbol <- (arbol (territorio $?t) (nodo ?nodoestado))
      ?explotada <- (casilla-explotada (indice ?indice) (nodo ?nodo))
      (test (and (neq ?nodo 0) (= ?nodo ?nodoestado) (< (+ ?indice 1) ?*maximo*)))

=>
      (printout t "Modificamos derecha " (+ ?indice 1) "" crlf)
      (assert (colindante (indice (+ ?indice 1)) (nodo ?nodo)))
)

(defrule explotar-izquierda
      (declare (salience 950))
      ?arbol <- (arbol (territorio $?t) (nodo ?nodoestado))
      ?explotada <- (casilla-explotada (indice ?indice) (nodo ?nodo))
      (test (and (neq ?nodo 0) (= ?nodo ?nodoestado) (> (- ?indice 1) 0)))

=>
      (printout t "Modificamos izquierda " (- ?indice 1) "" crlf)
      (assert (colindante (indice (- ?indice 1)) (nodo ?nodo)))
)

(defrule eliminar-casilla-explotada
      (declare (salience 900))
      ?arbol <- (arbol (territorio $?t) (nodo ?nodoestado))
      ?explotada <- (casilla-explotada (indice ?indice) (nodo ?nodo))
      (test (and (neq ?nodo 0) (= ?nodo ?nodoestado)))
=>
      (printout t "Explotar colindantes del indice " ?indice " , nodo " ?nodo "" crlf)
      (retract ?explotada)
)

(defrule explotar-colindante-A-N
      (declare (salience 850))
      ?arbol <- (arbol (territorio $?t) (nodo ?nodoestado))
      ?casillacolindante <- (colindante (indice ?indice) (nodo ?nodo))
      (test (and (neq ?nodo 0) (= ?nodo ?nodoestado)))
=>
      (bind ?ne (nth$ ?indice ?t))

      (if (eq ?ne A) then
          (modify ?arbol (territorio (replace$ ?t ?indice ?indice N)))
      )

      (retract ?casillacolindante)
)