package example

import scala.concurrent.Future/*->scala::concurrent::Future.*//*->scala::concurrent::Future#*/

object Example/*<-example::Example.*/ { self/*<-local0*/ =>
  new scala.collection.mutable.Stack/*->scala::collection::mutable::Stack#*/[Int/*->scala::Int#*/]()
  def main/*<-example::Example.main().*/(args/*<-example::Example.main().(args)*/: Array/*->scala::Array#*/[String/*->scala::Predef.String#*/]): Unit/*->scala::Unit#*/ = {
    println/*->scala::Predef.println(+1).*/(1)
  }
  val x/*<-example::Example.x.*/ = scala.reflect.classTag/*->scala::reflect::package.classTag().*/[Int/*->scala::Int#*/]
}
