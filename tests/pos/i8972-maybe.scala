//> using options -Yexplicit-nulls
import language.experimental.magic
trait Num:
  type Nat

object IsInt:
  def unapply(using num: Num)(sc: num.Nat): Int? = ???

def test(using num: Num)(x: num.Nat) =
  x match
    case IsInt(i) =>
