package i26000mergeedge

import language.experimental.captureChecking
import caps.*

class File
trait Tag
class Box[A](val value: A)
class Ref(init: Int) extends Mutable:
  private var current = init
  def get: Int = current
  update def set(x: Int): Unit = current = x
class Label extends Control
object a extends File, Tag, SharedCapability
object y extends File, Tag, SharedCapability
object z extends File, Tag, SharedCapability

object Lib:
  def flag: Boolean = true
  // Union and intersection parameter types
  val mu = Box(if flag then (x: File^{a, y} | Int) => () else (x: File^{a, z} | Int) => ())
  val mi = Box(if flag then (x: File^{a, y} & Tag) => () else (x: File^{a, z} & Tag) => ())
  // Parameters with different names
  val mn = Box(if flag then (x: File^{a, y}) => () else (f: File^{a, z}) => ())
  // A val of function type and a lambda
  val f1 = (x: File^{a, y}) => ()
  val ms = Box(if flag then f1 else (x: File^{a, z}) => ())

object Use uses a:
  def use() =
    Lib.mu.value(a)
    Lib.mn.value(a)
    Lib.ms.value(a)

// Read-only and classified capabilities
object Readers:
  def readers(r: Ref^) =
    val gs = List((y: Ref^{r.rd}) => y.get, (y: Ref^{r.rd}) => y.get + 1)
    gs.head(r)
  def mkReaders(r: Ref^) = List((y: Ref^{r.rd}) => y.get, (y: Ref^{r.rd}) => y.get + 1)
  def useReaders(r: Ref^) = mkReaders(r).head(r)
  def classified(l: Label^) =
    val q = List([C^] => (b: Object^{C.only[Control]}) => 1, [C^] => (b: Object^{C.only[Control]}) => 2)
    q.head[{l}](l)
