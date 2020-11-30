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
            (default -1))
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
      (slot valor
            (type INTEGER)
            (default 0))
)

(defrule inicial
      ?x <- (initial-fact)
=>
      (assert (arbol (territorio N N N N) (padre sin-padre) (nodo 0) (nivel 0) (indice 1)))
      (assert (nodoactual 0))
      (assert (resultado))
      (retract ?x)
)

(defrule explotar-N-B
      ?hecho <- (arbol (territorio $?primeras ?N $?ultimas) (nivel ?nivel) (indice ?i) (nodo ?n))
      (test (eq ?N N))
=>
      (bind ?posicion (+ (length$ $?primeras) 1))
      ;(printout t "Hecho " ?hecho " Posicion " ?posicion "" crlf)
      ;(printout t "territorio " $?primeras ?N $?ultimas "" crlf)
      (assert (arbol (territorio $?primeras B $?ultimas) (padre ?hecho) (nivel (+ ?nivel 1)) (indice ?posicion)))  
      
      (assert (casilla-explotada (indice ?posicion)))
)

(defrule explotar-B-A
      ?hecho <- (arbol (territorio $?primeras ?B $?ultimas) (nivel ?nivel) (indice ?i) (nodo ?n))
      (test (eq ?B B))
=>
      (bind ?posicion (+ (length$ $?primeras) 1))
      ;(printout t "Posicion " ?posicion "" crlf)
      ;(printout t "Nuevo territorio " $?primeras A $?ultimas "" crlf)
      (assert (arbol (territorio $?primeras A $?ultimas) (padre ?hecho) (nivel (+ ?nivel 1)) (indice ?posicion)))  
      
      (assert (casilla-explotada (indice ?posicion)))
)

(defrule explotar-arriba
      (declare (salience 950))
      ?arbol <- (arbol (territorio $?t) (nodo ?nodoestado))
      ?explotada <- (casilla-explotada (indice ?indice) (nodo ?nodo))
      (test (and (neq ?nodo 0) (= ?nodo ?nodoestado) (> (- ?indice ?*columnas*) 0)))

=>
      ;(printout t "Modificamos arriba " (- ?indice ?*columnas*) "" crlf)
      (assert (colindante (indice (- ?indice ?*columnas*)) (nodo ?nodo)))
)

(defrule explotar-abajo
      (declare (salience 950))
      ?arbol <- (arbol (territorio $?t) (nodo ?nodoestado))
      ?explotada <- (casilla-explotada (indice ?indice) (nodo ?nodo))
      (test (and (neq ?nodo 0) (= ?nodo ?nodoestado) (< (+ ?indice ?*columnas*) ?*maximo*)))

=>
      ;(printout t "Modificamos abajo " (+ ?indice ?*columnas*) "" crlf)
      (assert (colindante (indice (+ ?indice ?*columnas*)) (nodo ?nodo)))
)

(defrule explotar-derecha
      (declare (salience 950))
      ?arbol <- (arbol (territorio $?t) (nodo ?nodoestado))
      ?explotada <- (casilla-explotada (indice ?indice) (nodo ?nodo))
      (test (and (neq ?nodo 0) (= ?nodo ?nodoestado) (neq (mod ?indice ?*columnas*) 0)))

=>
      ;(printout t "Modificamos derecha " (+ ?indice 1) "" crlf)
      (assert (colindante (indice (+ ?indice 1)) (nodo ?nodo)))
)

(defrule explotar-izquierda
      (declare (salience 950))
      ?arbol <- (arbol (territorio $?t) (nodo ?nodoestado))
      ?explotada <- (casilla-explotada (indice ?indice) (nodo ?nodo))
      (test (and (neq ?nodo 0) (= ?nodo ?nodoestado) (neq (mod ?indice ?*columnas*) 1)))

=>
      ;(printout t "Modificamos izquierda " (- ?indice 1) "" crlf)
      (assert (colindante (indice (- ?indice 1)) (nodo ?nodo)))
)

(defrule eliminar-casilla-explotada
      (declare (salience 900))
      ?arbol <- (arbol (territorio $?t) (nodo ?nodoestado))
      ?explotada <- (casilla-explotada (indice ?indice) (nodo ?nodo))
      (test (and (neq ?nodo 0) (= ?nodo ?nodoestado)))
=>
      (retract ?explotada)
)

(defrule explotar-colindante-A-N
      (declare (salience 850))
      ?arbol <- (arbol (territorio $?t) (nodo ?nodoestado) (valor ?valor))
      ?casillacolindante <- (colindante (indice ?indice) (nodo ?nodo))
      (test (and (neq ?nodo 0) (= ?nodo ?nodoestado)))
=>
      (bind ?ne (nth$ ?indice ?t))

      (if (eq ?ne A) then
          (modify ?arbol (valor (- ?valor 3)) (territorio (replace$ ?t ?indice ?indice N)))
      )

      (retract ?casillacolindante)
)

(defrule contador-nodos-valor
      (declare (salience 800))
      ?arbol <- (arbol (territorio $?t) (nodo ?nodoestado) (valor ?valor))
      ?hechonodo <- (nodoactual ?contador)
      (test(= ?nodoestado 0))
=>
      (bind ?nuevovalor ?valor)
      (progn$ (?var ?t)
          (if (eq ?var B) then
              (bind ?nuevovalor (+ ?nuevovalor 1))
          )
          (if (eq ?var A) then
              (bind ?nuevovalor (+ ?nuevovalor 3))
          )
      )

      ;(printout t "Territorio " ?t " valor " ?nuevovalor "" crlf)

      (assert (nodoactual (+ 1 ?contador)))
      (modify ?arbol (nodo ?contador) (valor ?nuevovalor))
      (retract ?hechonodo)
)

(defrule nodo-casilla-explotada
      (declare (salience 800))
      ?explotada <- (casilla-explotada (indice ?indice) (nodo ?nodo))
      ?hechonodo <- (nodoactual ?contador)
      (test(= ?nodo 0))
=>
      (modify ?explotada (nodo ?contador))
)

(defrule elimina-nodos-repetidos
      (declare (salience 2000))
      ?hecho1 <- (arbol (territorio $?t1) (nodo ?nodo1) (nivel ?n1) (padre ?p1))
      ?hecho2 <- (arbol (territorio $?t2) (nodo ?nodo2) (nivel ?n2) (padre ?p2))
      (test(eq ?t1 ?t2))
      (test(neq ?hecho1 ?hecho2))
      (test(>= ?n2 ?n1))
      (test(neq ?nodo1 0))
=>
      ;(printout t "*---------------COINDIDENCIA---------------*" crlf)
      ;(printout t "t1 " ?t1 " t2 " ?t2 "" crlf)
      ;(printout t "n2 " ?n2 " n1 " ?n1 "" crlf)
      ;(printout t "nodo1 " ?nodo1 " nodo2 " ?nodo2 "" crlf)
      ;(printout t "hecho1 " ?hecho1 " hecho2 " ?hecho2 "" crlf)
      (retract ?hecho2)
)    

(defrule resultado
      (declare (salience -1000))
      ?resultado <- (resultado)
      ?hecho1 <- (arbol (territorio $?t1) (valor ?valor1))
      (not (arbol (valor ?valor2&:(> ?valor2 ?valor1))))
=>
      (printout t "Pintar mejor " ?t1 " y su valor es " ?valor1 " hecho " ?hecho1 "" crlf)
      (retract ?resultado)
)