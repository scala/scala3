@annotation.experimental class C(x: () => Unit) extends caps.Pure // error

@annotation.experimental class D(@annotation.constructorOnly x: () => Unit) extends caps.Pure // ok

