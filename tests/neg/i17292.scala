

import annotation.experimental

class Foo { @experimental type Bar = (Int, String) }

val f: Foo = Foo()

def g: Tuple.Elem[f.Bar, 0] = ??? // error
