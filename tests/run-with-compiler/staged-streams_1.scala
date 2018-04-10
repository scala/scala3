import dotty.tools.dotc.quoted.Toolbox._
import scala.quoted._

object Test {

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
  case class Nested[A, B](producer: Producer[B], nestedf: B => StagedStream[A]) extends StagedStream[A]

  case class Stream[A](stream: StagedStream[Expr[A]]) {

     def fold[W: Type](z: Expr[W], f: ((Expr[W], Expr[A]) => Expr[W])): Expr[W] = {
       Var(z) { s: Var[W] => '{

           ~fold_raw[Expr[A]]((a: Expr[A]) => '{
               ~s.update(f(s.get, a))
           }, stream)

           ~s.get
         }
       }
     }

     def fold_raw[A](consumer: A => Expr[Unit], s: StagedStream[A]): Expr[Unit] = {
       s match {
         case Linear(producer) => {
           producer.card match {
             case Many =>
               producer.init(sp => '{
                 while(~producer.hasNext(sp)) {
                   ~producer.step(sp, consumer)
                 }
               })
             case AtMost1 =>
               producer.init(sp => '{
                 if (~producer.hasNext(sp)) {
                   ~producer.step(sp, consumer)
                 }
               })
           }
         }
         case nested: Nested[a, bt] => ???
//         {
//            fold_raw[bt](((e: bt) => fold_raw(consumer, nested.nestedf(e))), Linear(nested.producer))
//         }
       }
     }
  }

  object Stream {
    def of[A: Type](arr: Expr[Array[A]]): Stream[A] = {
      val prod = new Producer[Expr[A]] {
        type St = (Var[Int], Var[Int], Expr[Array[A]])

        val card = Many

        def init(k: St => Expr[Unit]): Expr[Unit] = {
          Var('{(~arr).length}) { n =>
            Var(0.toExpr){ i =>
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

  def test1() = Stream
    .of('{Array(1, 2, 3)})
    .fold('{0}, ((a: Expr[Int], b : Expr[Int]) => '{ ~a + ~b }))


  def main(args: Array[String]): Unit = {
    println(test1().run)
  }
}



