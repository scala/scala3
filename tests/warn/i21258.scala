import scala.language.`3.6-migration`

object Test {
  type MT[X] = X match { // warn
    case Int => String
  }

  def unboundUnreducibleSig[X](x: X): MT[X] = ???

  type MT2[X] = X match { // no warning
    case Int => String
    case String => Any
  }
}
