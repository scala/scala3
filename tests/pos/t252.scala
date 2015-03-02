abstract class Module {}

abstract class T {
  type moduleType <: Module
  val module: moduleType
}

abstract class Base {
  type mType = Module
  type tType = T { type moduleType <: mType }
}

abstract class Derived extends Base {

  val t: T = ???

  // trying a simple dependent closure body first
  def cont1[X, Y](x: X)(f: X => Y): Y = f(x)
  cont1(t)(x => x.module)

  // trying an indirectly dependent closure body first
  def cont2[X, Y](x: X)(f: X => Int => Y): Y = f(x)(1)
  cont2(t)(x => z => x.module)

  // trying the original, harder case next
  def f(inputs: List[tType]): Unit = {
    for (t <- inputs; m = t.module) { }
  }
}
