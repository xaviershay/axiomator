# axiomator

An experiment in creating a proof-assistant DSL for applying mathematical
axioms to expressions. The following program:

    solution = do
      initial "a(b + c)"

      apply axiomDistribute
      apply axiomCommuteSum

Generates the following report, listing the result of applying each axiom and a
summary of all axioms used:

    1. Identity:                     a = a
    2. Distributive law:             a*(b+c) = ab+ac
    3. Commutative law for addition: a+b = b+a

    a(b + c) ; Identity
    ab + ac  ; Distributive law
    ac + ab  ; Commutative law for addition
