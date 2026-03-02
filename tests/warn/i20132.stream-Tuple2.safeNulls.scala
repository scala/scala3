//> using options -Yexplicit-nulls -Yno-flexible-types

import scala.jdk.CollectionConverters.*

class Test2:
  def t1: Unit = {
    val xs = List.empty[(String, String)]
    xs.asJava.forEach { case (a, b) => // warn
      ()
    }
  }
