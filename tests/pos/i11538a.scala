package a:

  trait Printer[A]:
    def print(a: A): Unit

  given Printer[String]:
    def print(s: String) = println(s)

package b:

  import a.{given, *}

  object test:
    import scala.compiletime.{error, summonFrom}

    inline def summonStringPrinter =
      summonFrom {
        case given Printer[String] => ()
        case _ => error("Couldn't find a printer")
      }

    val summoned = summon[Printer[String]]
    val summonedFrom = summonStringPrinter
