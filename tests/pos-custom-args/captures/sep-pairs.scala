import caps.Mutable
import caps.any

class Ref extends Mutable:
  var x = 0
  def get: Int = x
  update def put(y: Int): Unit = x = y

case class Pair[+A, +B](fst: A, snd: B)

def mkPair: Pair[Ref^, Ref^] =
  val r1 = Ref()
  val r2 = Ref()
  val p_exact: Pair[Ref^{r1}, Ref^{r2}] = Pair(r1, r2)
  p_exact

def copyPair[C^, D^](consume p: Pair[Ref^{C}, Ref^{D}]): Pair[Ref^{C}, Ref^{D}] =
  val x: Ref^{C} = p.fst
  val y: Ref^{D} = p.snd
  Pair[Ref^{C}, Ref^{D}](x, y)

/* TODO: The following variants don't work

def copyPair1[C^, D^](consume p: Pair[Ref^{C}, Ref^{D}]): Pair[Ref^{C}, Ref^{D}] =
  val x: Ref^{C} = p.fst
  val y: Ref^{D} = p.snd
  Pair(x, y)

def copyPair2[C^, D^](consume p: Pair[Ref^{C}, Ref^{D}]): Pair[Ref^, Ref^] =
  val x: Ref^{C} = p.fst
  val y: Ref^{D} = p.snd
  Pair(x, y)

*/