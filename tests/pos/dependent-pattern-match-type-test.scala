//> using options -Xfatal-warnings

import scala.reflect.TypeTest

trait M:
  type Tree <: AnyRef
  type Apply <: Tree
  given TypeTest[Tree, Apply] = ???
  val Apply: ApplyModule
  trait ApplyModule:
    this: Apply.type =>
    def unapply(x: Apply): (Tree, Tree) = ???

  def quote(x: Tree) = x match
    case Apply(f, args) =>
      println(args)
    case _ =>