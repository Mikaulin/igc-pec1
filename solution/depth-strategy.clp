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
      (slot estado
            (type SYMBOL)
            (allowed-symbols pendiente explotado completado)
            (default pendiente))
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
      (assert (arbol (territorio $?primeras B $?ultimas) (padre ?hecho) (nivel (+ ?nivel 1)) (indice ?posicion) (estado explotado)))
)

(defrule explotar-B-A
      ?hecho <- (arbol (territorio $?primeras ?B $?ultimas) (nivel ?nivel) (indice ?i) (nodo ?n))
      (test (eq ?B B))
=>
      (bind ?posicion (+ (length$ $?primeras) 1))
      (assert (arbol (territorio $?primeras A $?ultimas) (padre ?hecho) (nivel (+ ?nivel 1)) (indice ?posicion) (estado explotado)))  
)

(defrule explotar-arriba
      (declare (salience 950))
      ?arbol <- (arbol (territorio $?t) (nodo ?nodo) (indice ?indice) (estado explotado))
      (test (and (neq ?nodo 0) (> (- ?indice ?*columnas*) 0)))

=>
      (assert (colindante (indice (- ?indice ?*columnas*)) (nodo ?nodo)))
)

(defrule explotar-abajo
      (declare (salience 950))
?arbol <- (arbol (territorio $?t) (nodo ?nodo) (indice ?indice) (estado explotado))
      (test (and (neq ?nodo 0) (< (+ ?indice ?*columnas*) ?*maximo*)))

=>
      (assert (colindante (indice (+ ?indice ?*columnas*)) (nodo ?nodo)))
)

(defrule explotar-derecha
      (declare (salience 950))
      ?arbol <- (arbol (territorio $?t) (nodo ?nodo) (indice ?indice) (estado explotado))
      (test (and (neq ?nodo 0) (neq (mod ?indice ?*columnas*) 0)))

=>
      (assert (colindante (indice (+ ?indice 1)) (nodo ?nodo)))
)

(defrule explotar-izquierda
      (declare (salience 950))
      ?arbol <- (arbol (territorio $?t) (nodo ?nodo) (indice ?indice) (estado explotado))
      (test (and (neq ?nodo 0) (neq (mod ?indice ?*columnas*) 1)))

=>
      (assert (colindante (indice (- ?indice 1)) (nodo ?nodo)))
)


(defrule explotar-colindante-A-N
      (declare (salience 850))
      ?arbol <- (arbol (territorio $?t) (nodo ?nodoestado) (valor ?valor))
      ?casillacolindante <- (colindante (indice ?indice) (nodo ?nodo))
      (test (and (neq ?nodo 0) (= ?nodo ?nodoestado)))
=>
      (bind ?ne (nth$ ?indice ?t))

      (if (eq ?ne A) then
          (modify ?arbol (valor (- ?valor 3)) (territorio (replace$ ?t ?indice ?indice N)) (estado completado))
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

      (assert (nodoactual (+ 1 ?contador)))
      (modify ?arbol (nodo ?contador) (valor ?nuevovalor))
      (retract ?hechonodo)
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