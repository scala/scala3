//> using options -Yexplicit-nulls -Yno-flexible-types

// Previously failed because the scrutinee under
// unsafeNulls/explicit-nulls/no-flexible-types
// is (String, String) | Null
// Need to strip the null before considering it exhaustivity checkable

import scala.language.unsafeNulls

import scala.jdk.CollectionConverters.*

class Test2:
  def t1: Unit = {
    val xs = List.empty[(String, String)]
    xs.asJava.forEach { case (a, b) =>
      ()
    }
  }
