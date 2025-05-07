//> using options -Wvalue-discard

transparent inline def toto: Any = 1
transparent inline def uhoh = 42: Unit // warn ??
def tata: Unit = toto // warn
def hmm: Unit = uhoh
def literally: Unit = 42 // warn pure discard
def funnily = 42: Unit // warn ??
