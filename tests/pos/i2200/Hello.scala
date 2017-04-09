package bar
import scala.language.higherKinds
class Fix[F[_]](unfix: F[Fix[F]])
object DocTree {
  def docTree(s: StreamTree[DocTree]): DocTree = new Fix(s: StreamTree[DocTree])
}
