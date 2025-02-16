import caps.Mutable
import caps.{cap, consume, use}

class Ref extends Mutable:
  var x = 0
  def get: Int = x
  mut def put(y: Int): Unit = x = y

case class Pair[+A, +B](fst: A, snd: B)

def mkPair: Pair[Ref^, Ref^] =
  val r1 = Ref()
  val r2 = Ref()
  val p_exact: Pair[Ref^{r1}, Ref^{r2}] = Pair(r1, r2)
  p_exact

def copyPair(@consume @use p: Pair[Ref^, Ref^]): Pair[Ref^, Ref^] =
  val x: Ref^{p.fst*} = p.fst
  val y: Ref^{p.snd*} = p.snd
  Pair(x, y)

