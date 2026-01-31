object Math {
  class G {
    def print(): Unit = ???
  }
  class GS extends G

  opaque type Geo  = G
  opaque type GeoS <: Geo = GS

  object Geo:
    def apply(): Geo = new G
    extension (g: Geo) def print(): Unit = println("Geo clone called")
    extension (g: Geo) def toString(): String = "I am Geo" // warn

  object GeoS:
    def apply(): GeoS = new GS
    extension (g: GeoS) def print(): Unit = println("GeoS clone called") // no warn
}

import Math.*

object Main {
  def main(args: Array[String]): Unit = {
    val g = Geo()
    val s = GeoS()
    g.print()
    s.print()
    println(g.toString())
  }
}

object Calculus_B:
  trait Printable:
    def print(): Unit = ()

  private class PrintableGeoS extends Printable

  opaque type GeoS <: Printable = PrintableGeoS

  object GeoS:
    def apply(): GeoS = PrintableGeoS()
    extension (g: GeoS) def print(): Unit = ??? // warn
    extension (g: GeoS) /*override*/ def toString(): String = ??? // warn (override is also error)

object Calculus_C:
  trait Printable:
    def print(): Unit = ()

  private class PrintableGeoS extends Printable

  type P = Printable
  opaque type GeoS <: P = PrintableGeoS

  object GeoS:
    def apply(): GeoS = PrintableGeoS()
    extension (g: GeoS) def print(): Unit = ??? // warn
    extension (g: GeoS) /*override*/ def toString(): String = ??? // warn (override is also error)
