//> using options -Yexplicit-nulls
import language.experimental.errorHandling

trait Peano:
  type Nat
  val Succ: SuccExtractor
  trait SuccExtractor:
    def unapply(nat: Nat): Nat?

object IntNums extends Peano:
  type Nat = Int
  object Succ extends SuccExtractor:
    def unapply(nat: Nat) = nat - 1

object Test:
  def main(args: Array[String]): Unit =
    (3: IntNums.Nat) match
      case IntNums.Succ(v) => println("matched: " + v)
      case _ => println("no match")
