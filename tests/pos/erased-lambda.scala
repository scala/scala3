import language.experimental.erasedDefinitions

class Ev

def methodWithErasedEv(erased ev: Ev, x: Int): Int = x + 2

val lambdaWithErasedEv: (erased e: Ev, i: Int) => Int =
  (erased ev, x) => x + 2
