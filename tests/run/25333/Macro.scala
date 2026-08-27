package vendor

import scala.quoted.*

trait Callback:
  def run(): Any

object Macro:
  inline def make: Callback = ${makeImpl}

  def makeImpl(using Quotes): Expr[Callback] = '{
    new Callback:
      class Worker   // inner class inside quote — position from macro JAR → NoSource
      def run(): Any = new Worker
  }
