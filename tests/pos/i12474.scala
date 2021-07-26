package bugreport

import scala.compiletime.erasedValue

trait Show[A]:
  def show(a: A): String

inline def showTuple[Types]: Show[Types] =
  inline erasedValue[Types] match
    case _: (head *: tail) =>
      val instance =
        new Show[head *: tail]:
          def show(tuple: head *: tail): String = "dummy"
      instance.asInstanceOf[Show[Types]]

@main def run() =
  showTuple[(Int, Int)]
