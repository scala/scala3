import dotty.tools.dotc.quoted.Toolbox._
import scala.quoted._

trait StagedStreams {

  // TODO: remove as it exists in Quoted Lib
  sealed trait Var[T] {
    def get: Expr[T]
    def update(x: Expr[T]): Expr[Unit]
  }

  object Var {
    def apply[T: Type, U](init: Expr[T])(body: Var[T] => Expr[U]): Expr[U] = '{
      var x = ~init
      ~body(
        new Var[T] {
          def get: Expr[T] = '(x)
          def update(e: Expr[T]): Expr[Unit] = '{ x = ~e }
        }
      )
    }
  }

  type Id[A] = A

  trait Producer[A] { self =>
    type St
    val card: Cardinality

    def init(k: St => Expr[Unit]): Expr[Unit]
    def step(st: St, k: (A => Expr[Unit])): Expr[Unit]
    def hasNext(st: St): Expr[Boolean]
  }

  trait Cardinality
  case object AtMost1 extends Cardinality
  case object Many extends Cardinality

  trait StagedStream[A]
  case class Linear[A](producer: Producer[A]) extends StagedStream[A]
  case class Nested[A, B](producer: Producer[A], nestedf: A => StagedStream[B]) extends StagedStream[B]

  case class Stream[A](stream: StagedStream[Expr[A]]) {
    // def fold[W](z: Expr[W], f: (Expr[W] => Expr[A] => Expr[W])): Expr[W] = ???

    // def fold_raw[W](z: Expr[W], update_acc: Expr[W] => Expr[Unit], f: (Expr[W] => Expr[A] => Expr[W])): Expr[Unit] = {
    //   def consume[A](consumer: A => Expr[Unit], stream: StagedStream[A]): Expr[Unit] = {
    //     stream match {
    //       case Linear(producer) => {
    //         producer.card match {
    //           case Many =>
    //             producer.init(sp => '{
    //               while(~producer.hasNext(sp)) {
    //                 ~producer.step(sp, consumer)
    //               }
    //             })
    //           case AtMost1 =>
    //             producer.init(sp => '{
    //               if (~producer.hasNext(sp)) {
    //                 ~producer.step(sp, consumer)
    //               }
    //             })
    //         }
    //       }
    //       case Nested(producer, nestedf) => {
    //          ??? //consume(((a) => consume(consumer, nestedf(a))), Linear[A](producer))
    //       }
    //     }
    //   }

    //   ??? // consume((a: Expr[A]) => '{ ~update_acc(f(z)(a)) }, stream)
    // }
  }

  object Stream {
    def of[A: Type](arr: Expr[Array[A]]): Stream[A] = {
      val prod = new Producer[Expr[A]] {
        type St = (Var[Int], Var[Int], Expr[Array[A]])

        val card = Many

        def init(k: St => Expr[Unit]): Expr[Unit] = {
          Var('{(~arr).length}) { n =>
            Var(0.toExpr){i =>
              k((i, n, arr))
            }
          }
        }

        def step(st: St, k: (Expr[A] => Expr[Unit])): Expr[Unit] = {
          val (i, _, arr) = st
          '{
              val el = (~arr).apply(~i.get)
              ~i.update('{ ~i.get + 1 })
              ~k('(el))
          }
        }

        def hasNext(st: St): Expr[Boolean] =  {
          val (i, n, _) = st
          '{
              (~i.get < ~n.get)
          }
        }
      }

      Stream(Linear(prod))
    }
  }
}

