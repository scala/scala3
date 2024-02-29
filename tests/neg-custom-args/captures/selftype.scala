@annotation.experimental class C(x: () => Unit) extends Pure // error

@annotation.experimental class D(@annotation.constructorOnly x: () => Unit) extends Pure // ok

