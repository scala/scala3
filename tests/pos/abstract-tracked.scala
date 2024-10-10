import scala.language.experimental.modularity
import scala.language.future

trait F:
  tracked val x: Int

trait G:
  tracked val y: Int

object Test:
  // val f : F(1) /*: F { val x: 1 }*/ = new F:
  //   val x: 1 = 1
  val f = new F:
    val x = 1
  val g = new G:
    val y: 2 = 2

  summon[f.x.type <:< 1]
  summon[g.y.type <:< 2]
