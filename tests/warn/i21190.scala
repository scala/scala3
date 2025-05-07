
//> using options -Werror

opaque type FromEnd = Int
object FromEnd:
  inline def apply(i: Int): FromEnd = i
  extension (fe: FromEnd)
    inline def value: Int = fe

// Warning appears when extension is in same namespace as opaque type
extension [A](a: Array[A])
  inline def apply(fe: FromEnd): A =
    a(a.length - 1 - FromEnd.value(fe))

class R:
  def run(): String =
    val xs = Array(1, 2, 3)

    s"""First element = ${xs(0)}
       |Last element = ${xs(FromEnd(0))}""".stripMargin

@main def test = println:
  R().run()
