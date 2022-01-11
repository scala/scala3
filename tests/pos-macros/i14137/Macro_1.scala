package x

import scala.quoted._

object Macro:

   inline def genOp(inline f:Int): Int = ${
      genOpImpl('f)
   }

   def genOpImpl(f: Expr[Int])(using Quotes): Expr[Int] = {

      def firstOp()(using Quotes): Expr[Int] =
      '{
        var x=1
        ${secondOp('x,f)}
      }

      def secondOp(x:Expr[Int], y:Expr[Int])(using Quotes): Expr[Int] =
         '{ $x + $y }

      firstOp()
   }
