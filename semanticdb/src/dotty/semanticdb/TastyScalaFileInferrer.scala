package dotty.semanticdb

import scala.tasty.Reflection

import scala.meta.internal.{semanticdb => s}
import scala.tasty.Reflection
import scala.tasty.file.TastyConsumer
import java.lang.reflect.InvocationTargetException

class TastyScalaFileInferrer extends TastyConsumer {
  /* Visitor over a tasty tree.
  Aims at finding the scala file from where this tree originated.
  */

  /* If a scala file was found sourcePath is Some(scalaFile),
  Otherwise None */
  var sourcePath: Option[String] = None
  final def apply(reflect: Reflection)(root: reflect.Tree): Unit = {
    import reflect._
    object ChildTraverser extends TreeTraverser {
      override def traverseTree(tree: Tree)(implicit ctx: Context): Unit =
        tree match {
          case IsClassDef(cdef) => {
            cdef.symbol.annots.foreach { annot =>
              annot match {
                case Apply(Select(New(t), _),
                                List(Literal(Constant.String(path))))
                    if t.symbol.name == "SourceFile" =>
                  // we found the path to a file. In this case, we do not need to
                  // continue traversing the tree
                  sourcePath = Some(path)
                case x => super.traverseTree(tree)
              }
              true
            }
          }
          case _ => {
            // If we already found a sourcePath in this tasty file,
            // we abort our search here to avoid spending too much time here
            if (sourcePath == None)
              super.traverseTree(tree)
            else
              ()
          }
        }
    }
    ChildTraverser.traverseTree(root)(reflect.rootContext)
  }
}
