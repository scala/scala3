package dotty.vendored
package pprint

import java.util.function.Predicate

// FIXME: this would stay local to REPL extra libraries and never upstreamed.
// TODO: move to a separate module as this is not pprint specific?
object ReflectiveFunctions {

  /** create a predicate from a function shaped object.
   * @note e.g. if a hook is required to be injected accross a reflective boundary where
   * two classloaders have a separate copy of the `scala.Function1` class.
   */
  def reflectivePredicate(fn: Any): Any => Boolean = {
    val cls = fn.getClass
    val method = cls.getMethod("apply", classOf[Any])
    (x: Any) => method.invoke(fn, x.asInstanceOf[AnyRef]).asInstanceOf[Boolean]
  }

}
