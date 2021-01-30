import java.net.URLClassLoader

import scala.quoted.*

object Macro { self =>
  inline def f: Any = ${ impl }

  def impl(using Quotes): Expr[Any] = {
    //println("======== "+self.getClass.getClassLoader.asInstanceOf[URLClassLoader].getURLs.mkString("; "))
    //println("  ====== "+Thread.currentThread().getContextClassLoader.asInstanceOf[URLClassLoader].getURLs.mkString("; "))
    assert(getClass.getClassLoader eq Thread.currentThread().getContextClassLoader,
      "Macro ClassLoader should be available as context ClassLoader")
    '{""}
  }
}
