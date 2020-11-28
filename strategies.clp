(clear)


(set-strategy breadth)
(get-strategy)

(defrule rule-A
   ?f <- (a)
    =>
   (printout t "Rule A fires with " ?f crlf)
)

(defrule rule-B
   ?f <- (b)
    =>
   (printout t "Rule B fires with " ?f crlf)
)

(defrule rule-A-and-B
   ?f1 <- (a)
   ?f2 <- (b)
    =>
   (printout t "Rule B fires with A =" ?f1 " and B = " ?f2 crlf)
)