import scala.language.experimental.modularity
import scala.language.future

trait F:
  tracked val a: Int

class G:
  val a: Int = 1

def Test =
  val g = new G
  summon[g.a.type <:< 1] // error
