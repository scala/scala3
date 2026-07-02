package dotty.vendored
package pprint
import scala.collection.mutable.ArraySeq
import scala.collection.immutable.HashMap
import scala.collection.immutable.HashSet
import java.util.function.Predicate

/**
  * A lazy AST representing pretty-printable text. Models `foo(a, b)`
  * `foo op bar`, and terminals `foo` in both lazy and eager forms
  */
sealed trait Tree
object Tree{

  /**
    * Foo(aa, bbb, cccc)
    */
  case class Apply(prefix: String,
                   body: Iterator[Tree]) extends Tree

  /**
    * LHS op RHS
    */
  case class Infix(lhs: Tree, op: String, rhs: Tree) extends Tree

  /**
    * "xyz"
    */
  case class Literal(body: String) extends Tree{
    val hasNewLine = body.exists(c => c == '\n' || c == '\r')
  }

  /**
    * x = y
    */
  case class KeyValue(key: String, value: Tree) extends Tree

  /**
    * xyz
    */
  case class Lazy(body0: Ctx => Iterator[String]) extends Tree

  case class Ctx(width: Int,
                 leftOffset: Int,
                 indentCount: Int,
                 indentStep: Int,
                 literalColor: fansi.Attrs,
                 applyPrefixColor: fansi.Attrs)
}

abstract class Walker{
  val tuplePrefix = "scala.Tuple"
  val lazyClassNames = Set(
    "scala.collection.immutable.Stream$Cons",
    "scala.collection.immutable.LazyList",
    "scala.collection.immutable.LazyListIterable"
  )

  def additionalHandlers: PartialFunction[Any, Tree]
  def treeify(x: Any, escapeUnicode: Boolean, showFieldNames: Boolean): Tree =
    treeify(x, escapeUnicode, showFieldNames, ProductSupport.neverUseProductToString)

  def treeify(x: Any, escapeUnicode: Boolean, showFieldNames: Boolean, useProductToString: Predicate[Any]): Tree = additionalHandlers.lift(x).getOrElse{
    def toStringTree(x: Any) = Tree.Lazy(ctx =>
      Iterator(
        x.toString.asInstanceOf[String | Null] match{
          case null => "null"
          case s => s
        }
      )
    )

    x match{

      case null => Tree.Literal("null")
      case x: Boolean => Tree.Literal(x.toString)
      case x: Char =>
        val sb = new StringBuilder
        sb.append('\'')
        Util.escapeChar(x, sb, escapeUnicode)
        sb.append('\'')
        Tree.Literal(sb.toString)

      case x: Byte => Tree.Literal(x.toString)
      case x: Short => Tree.Literal(x.toString)
      case x: Int => Tree.Literal(x.toString)
      case x: Long => Tree.Literal(x.toString + "L")
      case x: Float => Tree.Literal(x.toString + "F")
      case x: Double => Tree.Literal(x.toString)
      case x: String =>
        if (x.exists(c => c == '\n' || c == '\r')) Tree.Literal("\"\"\"" + x + "\"\"\"")
        else Tree.Literal(Util.literalize(x, escapeUnicode))

      case x: StringBuilder => treeify(x.toString, escapeUnicode, showFieldNames, useProductToString)

      case x: Symbol => Tree.Literal("'" + x.name)

      case x: scala.collection.Map[_, _] =>
        Tree.Apply(
          x match{
            // Common concrete subclasses of `Seq`, `Map`, and `Set` that aren't imported by default,
            // do we should just use the name of the factory object that is imported by default
            case _: HashMap[_, _] => "Map"
            case _ => StringPrefix(x)
          },
          x.iterator.flatMap { case (k, v) =>
            Seq(Tree.Infix(treeify(k, escapeUnicode, showFieldNames, useProductToString), "->", treeify(v, escapeUnicode, showFieldNames, useProductToString)))
          }
        )

      // Do not force Streams and LazyLists to be computed.
      // Unfortunately, they do not leak lazy/eagerness by design, so we cannot simply ask "are you still lazy?".
      // So we check their toString.
      case x: Iterable[_] if lazyClassNames(x.getClass.getName) && x.toString.contains("<not computed>") =>
        Tree.Literal(x.toString)

      case x: Iterable[_] =>
        Tree.Apply(
          x match{
            case _: ArraySeq[_] => "Seq"
            case _: HashSet[_] => "Set"
            case _ => StringPrefix(x)
          },
          x.iterator.map(x => treeify(x, escapeUnicode, showFieldNames, useProductToString))
        )

      case None => Tree.Literal("None")

      case it: Iterator[_] =>
        // required since 2.13
        if (it.isEmpty)
          Tree.Literal("empty iterator")
        else
          Tree.Literal("non-empty iterator")

      case x: Array[_] => Tree.Apply("Array", x.iterator.map(x => treeify(x, escapeUnicode, showFieldNames, useProductToString)))

      case x: Product =>
        val className = x.getClass.getName
        if (x.productArity == 0) Tree.Lazy(ctx => Iterator(x.toString))
        else if (!className.startsWith(tuplePrefix) && useProductToString.test(x)) toStringTree(x)
        else if(x.productArity == 2 && Util.isOperator(x.productPrefix)){
          Tree.Infix(
            treeify(x.productElement(0), escapeUnicode, showFieldNames, useProductToString),

            x.productPrefix,
            treeify(x.productElement(1), escapeUnicode, showFieldNames, useProductToString)
          )
        } else (className.startsWith(tuplePrefix), className.lift(tuplePrefix.length)) match{
          // leave out tuple1, so it gets printed as Tuple1(foo) instead of (foo)
          // Don't check the whole suffix, because of specialization there may be
          // funny characters after the digit
          case (true, Some('2' | '3' | '4' | '5' | '6' | '7' | '8' | '9')) =>
            Tree.Apply("", x.productIterator.map(x => treeify(x, escapeUnicode, showFieldNames, useProductToString)))

          case _ =>
            Tree.Apply(x.productPrefix, ProductSupport.treeifyProductElements(x, this, escapeUnicode, showFieldNames, useProductToString))
        }

      case x => toStringTree(x)
    }
  }


}
