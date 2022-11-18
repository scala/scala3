import scala.compiletime._

// works
val a = {
    given Int = 0
    summon[Int]
}

// doesn't
transparent inline def summonInt = {
    given Int = 0
    summonInline[Int]
}

val b = summonInt
