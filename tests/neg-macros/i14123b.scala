package x

import scala.quoted._

object Impl {

      sealed trait UpdateOp[+T]
      case class Assignment[T](value:Expr[T]) extends UpdateOp[T]
      case class Update(operation:Expr[Unit]) extends UpdateOp[Nothing]

      def genRead[B:Type](newBuilder: Expr[B],
                          readVal: (Expr[B]) => UpdateOp[B]
                          )(using Quotes): Expr[B] =
          '{
                var x = $newBuilder
                  ${readVal[B]('x) match { // error: method apply in trait Function1 does not take type parameters
                    case Assignment(value) => '{ x = $value } // error
                    case Update(operation) => operation // error
                  }}
                x
          }

}
