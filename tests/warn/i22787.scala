//> using options -Wunused:imports -Werror

object tpd:
  type Block = Trees.Block

object Trees:
  abstract case class Block(x: Int)
  private object Block
  val block = new Block(42) {}

def f(x: Any) =
  import tpd.Block
  //import Trees.Block
  x match
  case Block(_) => "yes"
  case _ => "no"

@main def test = println:
  f(Trees.block)
