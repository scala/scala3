import scala.language.experimental.modularity
import scala.language.future

trait F:
  tracked val a: Int

trait G:
  tracked val b: Int

trait H:
  tracked val c: Int = 3

trait I extends F

trait J extends F:
  val a: Int = 1

class K(tracked val d: Int)

class L

trait M:
  val f: Int

class N extends F:
  val a = 10

object Test:
  val f = new F:
    val a = 1
  val g = new G:
    val b: 2 = 2
  val h = new H:
    override val c = 4
  val i = new I:
    val a = 5
  val j = new J:
    override val a = 6
  val k = new K(7)
  val l = new L {
    tracked val e = 8
  }
  val m = new M:
    tracked val f = 9
  val n = new N

  summon[f.a.type <:< 1]
  summon[g.b.type <:< 2]
  summon[h.c.type <:< 4]
  summon[i.a.type <:< 5]
  summon[j.a.type <:< 6]
  summon[k.d.type <:< 7]
  // summon[l.e.type <:< 8] // unrelated issue -- error: e is not a member of L
  summon[m.f.type <:< 9]
  summon[n.a.type <:< 10]
