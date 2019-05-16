import scala.tasty.Reflection
import scala.tasty.file._

object Test {
  def main(args: Array[String]): Unit = {
    ConsumeTasty("", List("pack.Foo"), new DBConsumer)
  }
}

class DBConsumer extends TastyConsumer {

  final def apply(reflect: Reflection)(root: reflect.Tree): Unit = {
    import reflect._
    object Traverser extends TreeTraverser {

      override def traverseTree(tree: Tree)(implicit ctx: Context): Unit = tree match {
        case IsClassDef(tree) =>
          super.traverseTree(tree)
        case IsTypeDef(tree) =>
          println("got type def")
          tree.rhs match {
            case IsTypeTree(ttree) =>
              println(ttree.show)
              ttree.tpe match {
                case IsType(_) => ;
                case IsTypeBounds(_) => sys.error("this should never happen")
              }
              super.traverseTree(ttree)
            case _ => ;
          }
          super.traverseTree(tree)
        case tree =>
          super.traverseTree(tree)
      }

    }
    Traverser.traverseTree(root)(reflect.rootContext)
  }

}
