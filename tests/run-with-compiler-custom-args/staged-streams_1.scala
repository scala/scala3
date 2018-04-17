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

     private def fold_raw[A](consumer: A => Expr[Unit], s: StagedStream[A]): Expr[Unit] = {
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
         case nested: Nested[a, bt] => {
            fold_raw[bt](((e: bt) => fold_raw[a](consumer, nested.nestedf(e))), Linear(nested.producer))
         }
       }
     }

    def map[B : Type](f: (Expr[A] => Expr[B])): Stream[B] = {
      Stream(mapRaw[Expr[A], Expr[B]](a => k => '{ ~k(f(a)) }, stream))
    }

    private def mapRaw[A, B](f: (A => (B => Expr[Unit]) => Expr[Unit]), stream: StagedStream[A]): StagedStream[B] = {
      stream match {
        case Linear(producer) => {
          val prod = new Producer[B] {

            type St = producer.St
            val card = producer.card

            def init(k: St => Expr[Unit]): Expr[Unit] = {
              producer.init(k)
            }

            def step(st: St, k: (B => Expr[Unit])): Expr[Unit] = {
              producer.step(st, el => f(el)(k))
            }

            def hasNext(st: St): Expr[Boolean] = {
              producer.hasNext(st)
            }
          }

          Linear(prod)
        }
        case nested: Nested[a, bt] => {
          Nested(nested.producer, (a: bt) => mapRaw[A, B](f, nested.nestedf(a)))
        }
      }
    }

    def flatMap[B : Type](f: (Expr[A] => Stream[B])): Stream[B] = {
      Stream(flatMapRaw[Expr[A], Expr[B]]((a => { val Stream (nested) = f(a); nested }), stream))
    }

    private def flatMapRaw[A, B](f: (A => StagedStream[B]), stream: StagedStream[A]): StagedStream[B] = {
      stream match {
        case Linear(producer) => Nested(producer, f)
        case nested: Nested[a, bt] =>
          Nested(nested.producer, (a: bt) => flatMapRaw[A, B](f, nested.nestedf(a)))
      }
    }

    def filter(f: (Expr[A] => Expr[Boolean])): Stream[A] = {
      val filterStream = (a: Expr[A]) =>
        new Producer[Expr[A]] {
          type St = Expr[A]
          val card = AtMost1

          def init(k: St => Expr[Unit]): Expr[Unit] =
            k(a)

          def step(st: St, k: (Expr[A] => Expr[Unit])): Expr[Unit] =
            k(st)

          def hasNext(st: St): Expr[Boolean] =
            f(st)
        }

      Stream(flatMapRaw[Expr[A], Expr[A]]((a => { Linear(filterStream(a)) }), stream))
    }

    private def moreTermination[A](f: Expr[Boolean] => Expr[Boolean], stream: StagedStream[A]): StagedStream[A] = {
      def addToProducer[A](f: Expr[Boolean] => Expr[Boolean], producer: Producer[A]): Producer[A] = {
        producer.card match {
            case Many =>
              new Producer[A] {
                type St = producer.St
                val card = producer.card

                def init(k: St => Expr[Unit]): Expr[Unit] =
                  producer.init(k)

                def step(st: St, k: (A => Expr[Unit])): Expr[Unit] =
                  producer.step(st, el => k(el))

                def hasNext(st: St): Expr[Boolean] =
                  f(producer.hasNext(st))
              }
            case AtMost1 => producer
        }
      }

      stream match {
        case Linear(producer) => Linear(addToProducer(f, producer))
        case nested: Nested[a, bt] =>
          Nested(addToProducer(f, nested.producer), (a: bt) => moreTermination(f, nested.nestedf(a)))
      }
    }

    private def addCounter[A](n: Expr[Int], producer: Producer[A]): Producer[(Var[Int], A)] = {
      new Producer[(Var[Int], A)] {
        type St = (Var[Int], producer.St)
        val card = producer.card

        def init(k: St => Expr[Unit]): Expr[Unit] = {
          producer.init(st => {
            Var(n) { counter =>
              k(counter, st)
            }
          })
        }

        def step(st: St, k: (((Var[Int], A)) => Expr[Unit])): Expr[Unit] = {
          val (counter, nst) = st
          producer.step(nst, el => '{
            ~k((counter, el))
          })
        }

        def hasNext(st: St): Expr[Boolean] = {
          val (counter, nst) = st
          producer.card match {
            case Many => '{ ~counter.get > 0 && ~producer.hasNext(nst) }
            case AtMost1 => '{ ~producer.hasNext(nst) }
          }
        }
      }
    }

    private def takeRaw[A](n: Expr[Int], stream: StagedStream[A]): StagedStream[A] = {
      stream match {
        case Linear(producer) => {
          mapRaw[(Var[Int], A), A]((t: (Var[Int], A)) => k => '{
            ~t._1.update('{~t._1.get - 1})
            ~k(t._2)
          }, Linear(addCounter(n, producer)))
        }
        case nested: Nested[a, bt] => {
          Nested(addCounter(n, nested.producer), (t: (Var[Int], bt)) => {
            mapRaw[A, A]((el => k => '{
              ~t._1.update('{~t._1.get - 1})
              ~k(el)
            }), moreTermination(b => '{ ~t._1.get > 0 && ~b}, nested.nestedf(t._2)))
          })
        }
      }
     }

    def take(n: Expr[Int]): Stream[A] = Stream(takeRaw[Expr[A]](n, stream))

    private def zipRaw[A, B](stream1: StagedStream[A], stream2: StagedStream[B]): StagedStream[(A, B)] = {
      (stream1, stream2) match {

        case (Linear(producer1), Linear(producer2)) =>
          Linear(zip_producer(producer1, producer2))

        case (Linear(producer1), Nested(producer2, nestf2)) =>
          pushLinear(producer1, producer2, nestf2)

        case (Nested(producer1, nestf1), Linear(producer2)) => ???

        case (Nested(producer1, nestf1), Nested(producer2, nestf2)) => ???
      }
    }

    private def pushLinear[A, B, C](producer: Producer[A], nestedProducer: Producer[B], nestedf: (B => StagedStream[C])): StagedStream[(A, C)] = {
      val newProducer = new Producer[(Var[Boolean], producer.St, B)] {

        type St = (Var[Boolean], producer.St, nestedProducer.St)
        val card: Cardinality = Many

        def init(k: St => Expr[Unit]): Expr[Unit] = {
          producer.init(s1 => '{ ~nestedProducer.init(s2 =>
            Var('{ ~producer.hasNext(s1) }) { term1r =>
              k((term1r, s1, s2))
            })})
        }

        def step(st: St, k: ((Var[Boolean], producer.St, B)) => Expr[Unit]): Expr[Unit] = {
          val (flag, s1, s2) = st
          nestedProducer.step(s2, b => '{ ~k((flag, s1, b)) })
        }

        def hasNext(st: St): Expr[Boolean] = {
          val (flag, s1, s2) = st
          '{ ~flag.get && ~nestedProducer.hasNext(s2) }
        }
      }

      Nested(newProducer, (t: (Var[Boolean], producer.St, B)) => {
        val (flag, s1, b) = t

        mapRaw[C, (A, C)]((c => k => '{
          ~producer.step(s1, a => '{ ~k((a, c)) })
          ~flag.update(producer.hasNext(s1))
        }), moreTermination((b_flag: Expr[Boolean]) => '{ ~flag.get && ~b_flag }, nestedf(b)))
      })
    }

    private def zip_producer[A, B](producer1: Producer[A], producer2: Producer[B]) = {
      new Producer[(A, B)] {

        type St = (producer1.St, producer2.St)
        val card: Cardinality = Many

        def init(k: St => Expr[Unit]): Expr[Unit] = {
          producer1.init(s1 => '{ ~producer2.init(s2 => '{ ~k((s1, s2)) })})
        }

        def step(st: St, k: ((A, B)) => Expr[Unit]): Expr[Unit] = {
          val (s1, s2) = st
          producer1.step(s1, el1 => '{ ~producer2.step(s2, el2 => '{ ~k((el1, el2)) })})
        }

        def hasNext(st: St): Expr[Boolean] = {
          val (s1, s2) = st
          '{ ~producer1.hasNext(s1) && ~producer2.hasNext(s2) }
        }
      }
    }

    def zip[B : Type, C : Type](f: (Expr[A] => Expr[B] => Expr[C]), stream2: Stream[B]): Stream[C] = {

      val Stream(stream_b) = stream2

      Stream(mapRaw[(Expr[A], Expr[B]), Expr[C]]((t => k => '{ ~k(f(t._1)(t._2)) }), zipRaw[Expr[A], Expr[B]](stream, stream_b)))
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

  def test2() = Stream
    .of('{Array(1, 2, 3)})
    .map((a: Expr[Int]) => '{ ~a * 2 })
    .fold('{0}, ((a: Expr[Int], b : Expr[Int]) => '{ ~a + ~b }))

  def test3() = Stream
    .of('{Array(1, 2, 3)})
    .flatMap((d: Expr[Int]) => Stream.of('{Array(1, 2, 3)}).map((dp: Expr[Int]) => '{ ~d * ~dp }))
    .fold('{0}, ((a: Expr[Int], b : Expr[Int]) => '{ ~a + ~b }))

  def test4() = Stream
    .of('{Array(1, 2, 3)})
    .filter((d: Expr[Int]) => '{ ~d % 2 == 0 })
    .fold('{0}, ((a: Expr[Int], b : Expr[Int]) => '{ ~a + ~b }))

  def test5() = Stream
    .of('{Array(1, 2, 3)})
    .take('{2})
    .fold('{0}, ((a: Expr[Int], b : Expr[Int]) => '{ ~a + ~b }))

  def test6() = Stream
    .of('{Array(1, 1, 1)})
    .flatMap((d: Expr[Int]) => Stream.of('{Array(1, 2, 3)}).take('{2}))
    .take('{5})
    .fold('{0}, ((a: Expr[Int], b : Expr[Int]) => '{ ~a + ~b }))

  def test7() = Stream
    .of('{Array(1, 2, 3)})
    .zip(((a : Expr[Int]) => (b : Expr[Int]) => '{ ~a + ~b }), Stream.of('{Array(1, 2, 3)}))
    .fold('{0}, ((a: Expr[Int], b : Expr[Int]) => '{ ~a + ~b }))

  def test8() = Stream
    .of('{Array(1, 2, 3)})
    .zip(((a : Expr[Int]) => (b : Expr[Int]) => '{ ~a + ~b }), Stream.of('{Array(1, 2, 3)}).flatMap((d: Expr[Int]) => Stream.of('{Array(1, 2, 3)}).map((dp: Expr[Int]) => '{ ~d + ~dp })))
    .fold('{0}, ((a: Expr[Int], b : Expr[Int]) => '{ ~a + ~b }))

  def main(args: Array[String]): Unit = {
    println(test1().run)
    println
    println(test2().run)
    println
    println(test3().run)
    println
    println(test4().run)
    println
    println(test5().run)
    println
    println(test6().run)
    println
    println(test7().run)
    println
    println(test8().run)
  }
}



