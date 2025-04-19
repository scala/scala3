//> using options -Wvalue-discard

transparent inline def toto: Any = 1
transparent inline def uhoh = 42: Unit // nowarn
def tata: Unit = toto // warn pure Discard
def hmm: Unit = uhoh
def literally: Unit = 42 // warn pure Discard
def funnily = 42: Unit // nowarn
def impure = ("*" * 42).length
def impurely: Unit = impure // warn impure discard
