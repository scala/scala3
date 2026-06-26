// https://github.com/scala/scala3/issues/17243
object Opaque:
  opaque type A = Int

  val va: A = 1

  inline def a(x: A) =
    x + 1

object Opaque2:
  opaque type B = Opaque.A

  val vb: B = Opaque.va

  inline def b(x: B) = Opaque.a(x)

@main def test17243() =
  print(Opaque2.b(Opaque2.vb))
