(ns vector.protocol)

(comment
  (require '[vector.protocol] :reload)
  (in-ns 'vector.protocol)
  )

(defprotocol MyNumber
  "What makes a number a number"
  (add [_ n] "addition of number")
  (mult [_ n]"multiplication of number")
  (negative [_] "the negativve of a number")
  (reciprocal [_] "the reciprocal of a nimber")
  (conjugate [-] "the conjugate of a number")
  (evaluate [_] "evaluate to a double")
  (equals [_ other] "determines if this number equals the other"))
