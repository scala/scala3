package i14393
import scala.quoted.*

object M {

  inline def useFoldTree[X](inline x:X):X = ${
     useFoldTreeImpl('x)
  }

  def useFoldTreeImpl[X:Type](x:Expr[X])(using Quotes):Expr[X] = {
     import quotes.reflect.*
     val search = new TreeAccumulator[Int] {
        def foldTree(s:Int, tree: Tree)(owner: Symbol): Int =
                 foldOverTree(s,tree)(owner)
     }
     search.foldTree(0,x.asTerm)(Symbol.spliceOwner)
     x
  }

}
