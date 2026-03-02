class C:
  class D

class Baz:
  val c = C()
  export c.*

@main def Test = Baz().D() // error (Baz#c : C) is not a valid class prefix, since it is not an immutable path
