
package reproduction.opaqueoverwarning

opaque type FromEnd = Int
object FromEnd:
  inline def apply(i: Int): FromEnd = i
  extension (fe: FromEnd)
    inline def value: Int = fe

// Warning appears when extension is in same namespace as opaque type
// under 3.5.0-RC3 (and 3.6.0 nightly)
extension [A](a: Array[A])
  inline def apply(fe: FromEnd): A =
    a(a.length - 1 - FromEnd.value(fe))

object Main:
  def run(): Unit =
    val xs = Array(1, 2, 3)

    println(s"First element = ${xs(0)}")
    println(s"Last element = ${xs(FromEnd(0))}")

  def main(args: Array[String]): Unit =
    run()
