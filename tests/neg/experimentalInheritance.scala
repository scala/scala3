import scala.annotation.experimental

@experimental
class A

@experimental
trait T

class B extends A // error

@experimental
class B2 extends A

class C extends T // error

@experimental
class C2 extends T

@experimental
class O:
  class X

  @experimental
  class Y

  object Z

@experimental
object O:
  class A

  @experimental
  class B

  object C

class OA extends O.A // error
class OB extends O.B // error

@experimental
class OA2 extends O.A

@experimental
class OB2 extends O.B
