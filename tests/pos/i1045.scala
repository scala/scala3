import scala.collection.*

object EmptyHashMap extends mutable.HashMap[Nothing, Nothing]
object T {
  val newSymbolMap: mutable.HashMap[String, mutable.HashMap[Int, Double]] = mutable.HashMap.empty
  val map = newSymbolMap.getOrElse("a", mutable.HashMap.empty)
  map.put(1, 0.0)
  newSymbolMap.put("a", map)

   /** A map storing free variables of functions and classes */
//  type SymSet = Set[Symbol]
//  private val free = new collection.mutable.LinkedHashMap[Symbol, SymSet]
//  def freeVars(sym: Symbol): List[Symbol] = free.getOrElse(sym, Nil).toList

  class Tree[X >: Null] { def tpe: X = null }
  class Ident[X >: Null] extends Tree[X]
  class Apply[X >: Null] extends Tree[X]

  val x: Ident[Symbol] | Apply[Symbol] = ???
  val y = x.tpe
  val z: Symbol = y


}
