inline def macroWithAssertFailing[T](t: T): Unit = ${ macroWithAssertFailingImpl[T]('t) }

@main
def run =
  macroWithAssertFailing[Int](123) // error

