import scala.language.experimental.modularity
import scala.language.future

trait F:
  tracked val x: Int

trait G:
  tracked val y: Int

trait H:
  tracked val z: Int = 3

trait I extends F

trait J extends F:
  val x: Int = 1

class K(tracked val x: Int)

object Test:
  // val f : F(1) /*: F { val x: 1 }*/ = new F:
  //   val x: 1 = 1
  val f = new F:
    val x = 1
  val g = new G:
    val y: 2 = 2
  val h = new H:
    override val z = 4
  val i = new I:
    val x = 5
  val j = new J:
    override val x = 6
  val k = new K(7)

  summon[f.x.type <:< 1]
  summon[g.y.type <:< 2]
  summon[h.z.type <:< 4]
  summon[i.x.type <:< 5]
  summon[j.x.type <:< 6]
  summon[k.x.type <:< 7]
