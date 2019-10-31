val a = 0.25: BigDecimal
val b = 0xcafebabe: BigDecimal // error
val c = 1.3: BigInt // error
val d = 2e500: BigInt // error

val e = (??? : Any) match
  case 0xcafebabe: BigDecimal => // error
    ()

val f = (??? : Any) match
  case 1.3: BigInt => // error
    ()

val g = (??? : Any) match
  case 2e500: BigInt => // error
    ()

val h = (1.3: BigDecimal) match
  case 0xa => // error
    ()

val i = (1: BigInt) match
  case 1.2 => // error
    ()

val j = (1: BigInt) match
  case 5e10 => // error
    ()
