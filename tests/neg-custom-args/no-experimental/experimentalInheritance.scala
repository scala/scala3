import scala.annotation.experimental

@experimental def x = 2

@experimental class A1(x: Any)
class A2(x: Any)


@experimental class B1 extends A1(1)
class B2 // error: extension of experimental class A1 must have @experimental annotation
extends A1(1) // error: class A1 is marked @experimental ...

@experimental class C1 extends A2(x)
class C2 extends A2(x) // error def x is marked @experimental and therefore
