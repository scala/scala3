import scala.quoted.*

inline def foo(f: Int => Int): Int => Int = ${impl('f)}
inline def bar(inline f: Int => Int): Int => Int = ${impl('f)}
inline def baz(inline f: (Int => Int)*): Int => Int = ${impl2('f)}

def impl(f: Expr[Int => Int])(using Quotes): Expr[Int => Int] =
  assertNoNamedArgs(f)
  '{identity}

def impl2(f: Expr[Seq[Int => Int]])(using Quotes): Expr[Int => Int] =
  assertNoNamedArgs(f)
  '{identity}

def assertNoNamedArgs(expr: Expr[Any])(using Quotes): Unit =
  import quotes.reflect.*
  new TreeTraverser {
    override def traverseTree(tree: Tree)(owner: Symbol): Unit = tree match
      case _: NamedArg =>
        report.throwError(s"Unexpected NamedArg after inlining: ${tree}", tree.pos)
      case _ => traverseTreeChildren(tree)(owner)
  }.traverseTree(expr.asTerm)(Symbol.spliceOwner)
