object Opaque:
  opaque type A = Int

  val va: A = 1

  inline def a(x: A) =
    x + 1

object Opaque2:
  opaque type B = Opaque.A

  val vb: B = Opaque.va

  inline def b(x: B) = Opaque.a(x)

@main def Test() =
  print(Opaque2.b(Opaque2.vb))
