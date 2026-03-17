// scalajs: --skip
import scala.annotation.unchecked.uncheckedVariance

trait SubFlowDef[+F[+_]]
final class Flow1[-In]{
  type Repr[+O] = Flow1[In @uncheckedVariance]
}
final class Flow2[+Out]{
  type Repr[+O] = Flow2[O]
}
class SubFlow[In, Out](
    val delegate1: SubFlowDef[Flow1[In]#Repr],
    val delegate2: SubFlowDef[Flow2[Out]#Repr]
)

object Test {
  def main(args: Array[String]): Unit = {
    classOf[SubFlow[?, ?]]
      .getConstructors()
      .map(_.toGenericString())
      .sorted
      .foreach(println)

    classOf[SubFlow[?, ?]]
      .getMethods()
      .filter(_.getName().startsWith("delegate"))
      .map(_.toGenericString())
      .sorted
      .foreach(println)
  }
}
