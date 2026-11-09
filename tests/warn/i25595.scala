val avs = ((0d, BigDecimal(1)), (1d, BigDecimal(2)))

val ((x1, f1), (x2, f2)): ( // warn
  (Double, BigDecimal),
  (Double, BigDecimal)
) = avs

def notVar(i: Int) = i match
  case X: Int => X // warn

def notVarBackquoted(i: Int) = i match
  case `Int`: Int => `Int` // warn
