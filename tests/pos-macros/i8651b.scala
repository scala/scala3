abstract class Coroutine[+T] {
  def continue: Option[T]
}

object Macros {

 import scala.quoted._

 inline def coroutine[T](inline body: Any): Coroutine[T] = ${ coroutineImpl('{body}) }

 def coroutineImpl[T](using s: Scope)(expr: s.Expr[Any])(using s.Type[T]): s.Expr[Coroutine[T]] = {
   '{
     new Coroutine[T] {
       var state: Int = 0

       def continue: Option[T] = ${
         '{
           state = 1 //if this line is commented there are no compile errors anymore!!!
           None
         }
       }

     }
   }

 }
}
