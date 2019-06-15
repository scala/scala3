import scala.quoted._
import scala.quoted.autolift._

/**
  * Port of the strymonas library as described in O. Kiselyov et al., Stream fusion, to completeness (POPL 2017)
  */

object Test {

  // TODO: remove as it exists in Quoted Lib
  sealed trait Var[T] {
    def get: Expr[T]
    def update(x: Expr[T]): Expr[Unit]
  }

  object Var {
    def apply[T: Type, U: Type](init: Expr[T])(body: Var[T] => Expr[U]): Expr[U] = '{
      var x = $init
      ${
        body(
          new Var[T] {
            def get: Expr[T] = 'x
            def update(e: Expr[T]): Expr[Unit] = '{ x = $e }
          }
        )
      }
    }
  }

  /*** Producer represents a linear production of values with a loop structure.
    *
    * Conceptually the design of the producer has its roots in `unfold` where a stream is a product type of some state
    * and a stepper function:
    *
    * {{
    *   trait Stream[+A]
    *   case class Unfold[S, +A](state: S, step: (S) => Option[(S, A)]) extends Stream[+A]
    * }}
    *
    * The latter transforms the state and returns either the end-of-the-stream or a value and
    * the new state. The existential quantification over the state keeps it private: the only permissible operation is
    * to pass it to the function.
    *
    * Note: in `Producer` the elements are not pulled but the step accepts a continuation.
    *
    * A Producer defines the three basic elements of a loop structure:
    * - `init` contributes the code before iteration starts
    * - `step` contributes the code during execution
    * - `hasNext` contributes the code of the boolean test to end the iteration
    *
    * @tparam A type of the collection element. Since a `Producer` is polymorphic it handles `Expr` values, we
    *           can pack together fragments of code to accompany each element production (e.g., a variable incremented
    *           during each transformation)
    */
  trait Producer[A] { self =>
    type St
    val card: Cardinality

    /** Initialization method that defines new state, if needed by the combinator that this producer defines.
      *
      * @param  k the continuation that is invoked after the new state is defined in the body of `init`
      * @return expr value of unit per the CPS-encoding
      */
    def init(k: St => Expr[Unit]): Expr[Unit]

    /** Step method that defines the transformation of data.
      *
      * @param  st the state needed for this iteration step
      * @param  k  the continuation that accepts each element and proceeds with the step-wise processing
      * @return expr value of unit per the CPS-encoding
      */
    def step(st: St, k: (A => Expr[Unit])): Expr[Unit]

    /** The condition that checks for termination
      *
      * @param  st the state needed for this iteration check
      * @return the expression for a boolean
      */
    def hasNext(st: St): Expr[Boolean]
  }

  trait Cardinality
  case object AtMost1 extends Cardinality
  case object Many extends Cardinality

  trait StagedStream[A]
  case class Linear[A](producer: Producer[A]) extends StagedStream[A]
  case class Nested[A, B](producer: Producer[B], nestedf: B => StagedStream[A]) extends StagedStream[A]

  case class Stream[A: Type](stream: StagedStream[Expr[A]]) {

    /** Main consumer
      *
      * Fold accumulates the results in a variable and delegates its functionality to `foldRaw`
      *
      * @param z   the accumulator
      * @param f   the zipping function
      * @tparam W  the type of the accumulator
      * @return
      */
    def fold[W: Type](z: Expr[W], f: ((Expr[W], Expr[A]) => Expr[W])): Expr[W] = {
      Var(z) { s: Var[W] => '{
          ${
            foldRaw[Expr[A]]((a: Expr[A]) => '{
              ${ s.update(f(s.get, a)) }
            }, stream)
          }
          ${ s.get }
        }
      }
    }

     private def foldRaw[A](consumer: A => Expr[Unit], stream: StagedStream[A]): Expr[Unit] = {
       stream match {
         case Linear(producer) => {
           producer.card match {
             case Many =>
               producer.init(sp => '{
                 while(${producer.hasNext(sp)}) {
                   ${producer.step(sp, consumer)}
                 }
               })
             case AtMost1 =>
               producer.init(sp => '{
                 if (${producer.hasNext(sp)}) {
                   ${producer.step(sp, consumer)}
                 }
               })
           }
         }
         case nested: Nested[A, bt] => {
           foldRaw[bt](((e: bt) => foldRaw[A](consumer, nested.nestedf(e))), Linear(nested.producer))
         }
       }
     }

    /** Builds a new stream by applying a function to all elements of this stream.
      *
      * @param  f the function to apply to each quoted element.
      * @tparam B the element type of the returned stream
      * @return   a new stream resulting from applying `mapRaw` and threading the element of the first stream downstream.
      */
    def map[B : Type](f: (Expr[A] => Expr[B])): Stream[B] = {
      Stream(mapRaw[Expr[A], Expr[B]](a => k => k(f(a)), stream))
    }

    /** Handles generically the mapping of elements from one producer to another.
      * `mapRaw` can be potentially used threading quoted values from one stream to another. However
      * is can be also used by declaring any kind of computation we need to perform during each step.
      *
      * e.g., `mapRaw[(Var[Int], A), A]` transforms a stream that declares a variable and holds a value in each
      * iteration step, to a stream that is not aware of the aforementioned variable.
      *
      * @param  f      the function to apply at each step. f is of type `(A => (B => Expr[Unit])` where A is the type of
      *                the incoming stream. When applied to an element, `f` returns the continuation for elements of `B`
      * @param  stream that contains the stream we want to map.
      * @tparam A      the type of the input stream
      * @tparam B      the element type of the resulting stream
      * @return        a new stream resulting from applying `f` in the `step` function of the input stream's producer.
      */
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
        case nested: Nested[A, bt] => {
          Nested(nested.producer, (a: bt) => mapRaw[A, B](f, nested.nestedf(a)))
        }
      }
    }

    /** Flatmap */
    def flatMap[B : Type](f: (Expr[A] => Stream[B])): Stream[B] = {
      Stream(flatMapRaw[Expr[A], Expr[B]]((a => { val Stream (nested) = f(a); nested }), stream))
    }

    /** Returns a new stream that applies a function `f` to each element of the input stream.
      * If the input stream is simply linear then its packed with the function `f`.
      * If the input stream is nested then a new one is created by using its producer and then passing the `f`
      * recursively to build the `nestedf` of the returned stream.
      *
      *    Note: always returns a nested stream.
      *
      * @param  f      the function of `flatMap``
      * @param  stream the input stream
      * @tparam A      the type of the input stream
      * @tparam B      the element type of the resulting stream
      * @return        a new stream resulting from registering `f`
      */
    private def flatMapRaw[A, B](f: (A => StagedStream[B]), stream: StagedStream[A]): StagedStream[B] = {
      stream match {
        case Linear(producer) => Nested(producer, f)
        case nested: Nested[a, bt] =>
          Nested(nested.producer, (a: bt) => flatMapRaw[A, B](f, nested.nestedf(a)))
      }
    }

    /** Selects all elements of this stream which satisfy a predicate.
      *
      *    Note: this is merely a special case of `flatMap` as the resulting stream in each step may return 0 or 1
      *    element.
      *
      * @param f    the predicate used to test elements.
      * @return     a new stream consisting of all elements of the input stream that do satisfy the given
      *             predicate `pred`.
      */
    def filter(pred: (Expr[A] => Expr[Boolean])): Stream[A] = {
      val filterStream = (a: Expr[A]) =>
        new Producer[Expr[A]] {

          type St = Expr[A]
          val card = AtMost1

          def init(k: St => Expr[Unit]): Expr[Unit] =
            k(a)

          def step(st: St, k: (Expr[A] => Expr[Unit])): Expr[Unit] =
            k(st)

          def hasNext(st: St): Expr[Boolean] =
            pred(st)
        }

      Stream(flatMapRaw[Expr[A], Expr[A]]((a => { Linear(filterStream(a)) }), stream))
    }

    /** Adds a new termination condition to a stream (recursively if nested) of cardinality `Many`.
      *
      * @param  condition      the termination condition as a function accepting the existing condition (the result
      *                of the `hasNext` from the passed `stream`'s producer.
      * @param  stream that contains the producer we want to enhance.
      * @tparam A      the type of the stream's elements.
      * @return        the stream with the new producer. If the passed stream was linear, the new termination is added
      *                otherwise the new termination is propagated to all nested ones, recursively.
      */
    private def addTerminationCondition[A](condition: Expr[Boolean] => Expr[Boolean], stream: StagedStream[A]): StagedStream[A] = {
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
        case Linear(producer) => Linear(addToProducer(condition, producer))
        case nested: Nested[a, bt] =>
          Nested(addToProducer(condition, nested.producer), (a: bt) => addTerminationCondition(condition, nested.nestedf(a)))
      }
    }

    /** Adds a new counter variable by enhancing a producer's state with a variable of type `Int`.
      * The counter is initialized in `init`, propageted in `step` and checked in the `hasNext` of the *current* stream.
      *
      * @param  n        is the initial value of the counter
      * @param  producer the producer that we want to enhance
      * @tparam A        the type of the producer's elements.
      * @return          the enhanced producer
      */
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
          val (counter, currentState) = st
          producer.step(currentState, el => '{
            ${k((counter, el))}
          })
        }

        def hasNext(st: St): Expr[Boolean] = {
          val (counter, currentState) = st
          producer.card match {
            case Many => '{ ${counter.get} > 0 && ${producer.hasNext(currentState)} }
            case AtMost1 => producer.hasNext(currentState)
          }
        }
      }
    }

    /** The nested stream receives the same variable reference; thus all streams decrement the same global count.
      *
      * @param  n      code of the variable to be threaded to the downstream.
      * @param  stream the upstream to enhance.
      * @tparam A      the type of the producer's elements.
      * @return        a linear or nested stream aware of the variable reference to decrement.
      */
    private def takeRaw[A: Type](n: Expr[Int], stream: StagedStream[A]): StagedStream[A] = {
      stream match {
        case linear: Linear[A] => {
          val enhancedProducer: Producer[(Var[Int], A)] = addCounter[A](n, linear.producer)
          val enhancedStream: Linear[(Var[Int], A)] = Linear(enhancedProducer)

          // Map an enhanced stream to a stream that produces the elements. Before
          // invoking the continuation for the element, "use" the variable accordingly.
          mapRaw[(Var[Int], A), A]((t: (Var[Int], A)) => k => '{
            ${t._1.update('{${t._1.get} - 1})}
            ${k(t._2)}
          }, enhancedStream)
        }
        case nested: Nested[A, bt] => {
          val enhancedProducer: Producer[(Var[Int], bt)] = addCounter[bt](n, nested.producer)

          Nested(enhancedProducer, (t: (Var[Int], bt)) => {
            // Before invoking the continuation for the element, "use" the variable accordingly.
            // In contrast to the linear case, the variable is initialized in the originating stream.
            mapRaw[A, A]((el => k => '{
              ${t._1.update('{${t._1.get} - 1})}
              ${k(el)}
            }), addTerminationCondition(b => '{ ${t._1.get} > 0 && $b}, nested.nestedf(t._2)))
          })
        }
      }
     }

    /** A stream containing the first `n` elements of this stream. */
    def take(n: Expr[Int]): Stream[A] = Stream(takeRaw[Expr[A]](n, stream))

    private def zipRaw[A: Type, B: Type](stream1: StagedStream[Expr[A]], stream2: StagedStream[B]): StagedStream[(Expr[A], B)] = {
      (stream1, stream2) match {

        case (Linear(producer1), Linear(producer2)) =>
          Linear(zip_producer(producer1, producer2))

        case (Linear(producer1), Nested(producer2, nestf2)) =>
          pushLinear[A = Expr[A], C = B](producer1, producer2, nestf2)

        case (Nested(producer1, nestf1), Linear(producer2)) =>
          mapRaw[(B, Expr[A]), (Expr[A], B)]((t => k => '{ ${k((t._2, t._1))} }), pushLinear[A = B, C = Expr[A]](producer2, producer1, nestf1))

        case (Nested(producer1, nestf1), Nested(producer2, nestf2)) =>
          zipRaw[A, B](Linear(makeLinear(stream1)), stream2)
      }
    }

    /** Make a stream linear
      *
      * Performs reification of the `stream`. It converts it to a function that will, when called, produce the current element
      * and advance the stream -- or report the end-of-stream.
      * The reified stream is an imperative *non-recursive* function, called `adv`, of `Unit => Unit` type. Nested streams are
      * also handled.
      *
      * @example {{{
      *
      *    Stream.of(1,2,3).flatMap(d => ...)
      *          .zip(Stream.of(1,2,3).flatMap(d => ...))
      *          .map{ case (a, b) => a + b }
      *          .fold(0)((a, b) => a + b)
      * }}}
      *
      * -->
      *
      * {{{
      *           /* initialization for stream 1 */
      *
      *           var curr = 0.asInstanceOf[Int]  // keeps each element from reified stream
      *           var nadv: Unit => Unit = (_) => () // keeps the advance for each nested level
      *
      *           def adv: Unit => Unit = {
      *              /* Linearization of stream1 - updates curr from stream1 */
      *              curr = ...
      *           }
      *           nadv = adv
      *           nadv()
      *
      *           /* initialization for stream 2 */
      *
      *           def outer () = {
      *               /* initialization for outer stream of stream 2 */
      *               def inner() = {
      *                 /* initialization for inner stream of stream 2 */
      *                 val el = curr
      *                 nadv()
      *                 /* process elements for map and fold */
      *                 inner()
      *               }
      *               inner()
      *               outer()
      *           }
      *           outer()
      * }}}
      *
      * @param  stream
      * @tparam A
      * @return
      */
    private def makeLinear[A: Type](stream: StagedStream[Expr[A]]): Producer[Expr[A]] = {
      stream match {
        case Linear(producer) => producer
        case Nested(producer, nestedf) => {
          /** Helper function that orchestrates the handling of the function that represents an `advance: Unit => Unit`.
            * It reifies a nested stream as calls to `advance`. Advance encodes the step function of each nested stream.
            * It is used in the init of a producer of a nested stream. When an inner stream finishes, the
            * `nadv` holds the function to the `advance` function of the earlier stream.
            * `makeAdvanceFunction`, for each nested stream, installs a new `advance` function that after
            * the stream finishes it will restore the earlier one.
            *
            * When `advance` is called the result is consumed in the continuation. Within this continuation
            * the resulting value should be saved in a variable.
            *
            * @param  nadv variable that holds a function that represents the stream at each level.
            * @param  k              the continuation that consumes a variable.
            * @return the quote of the orchestrated code that will be executed as
            */
          def makeAdvanceFunction[A](nadv: Var[Unit => Unit], k: A => Expr[Unit], stream: StagedStream[A]): Expr[Unit] = {
            stream match {
              case Linear(producer) =>
                producer.card match {
                  case AtMost1 => producer.init(st => '{
                    if(${producer.hasNext(st)}) {
                      ${producer.step(st, k)}
                    }
                    else {
                      val f = ${nadv.get}
                      f(())
                    }
                  })
                  case Many => producer.init(st => '{
                    val oldnadv: Unit => Unit = ${nadv.get}
                    val adv1: Unit => Unit = { _: Unit => {
                      if(${producer.hasNext(st)}) {
                        ${producer.step(st, k)}
                      }
                      else {
                        ${nadv.update('oldnadv)}
                        oldnadv(())
                      }
                    }}

                    ${nadv.update('adv1)}
                    adv1(())
                  })
                }
              case nested: Nested[A, bt] =>
                makeAdvanceFunction(nadv, (a: bt) => makeAdvanceFunction(nadv, k, nested.nestedf(a)), Linear(nested.producer))
            }
          }

          new Producer[Expr[A]] {
            // _1: if the stream has ended,
            // _2: the current element,
            // _3: the step of the inner most steam
            type St = (Var[Boolean], Var[A], Var[Unit => Unit])
            val card: Cardinality = Many

            def init(k: St => Expr[Unit]): Expr[Unit] = {
              producer.init(st =>
                Var('{ (_: Unit) => ()}){ nadv => {
                  Var('{ true }) { hasNext => {
                     Var('{ null.asInstanceOf[A] }) { curr => '{

                        // Code generation of the `adv` function
                        def adv: Unit => Unit = { _ =>
                          ${hasNext.update(producer.hasNext(st))}
                          if(${hasNext.get}) {
                            ${producer.step(st, el => {
                              makeAdvanceFunction[Expr[A]](nadv, (a => curr.update(a)), nestedf(el))
                            })}
                          }
                        }

                        ${nadv.update('adv)}
                        adv(())
                        ${k((hasNext, curr, nadv))}
                     }}
                  }}
                }})
            }

            def step(st: St, k: Expr[A] => Expr[Unit]): Expr[Unit] = {
              val (flag, current, nadv) = st
              '{
                var el = ${current.get}
                val f: Unit => Unit = ${nadv.get}
                f(())
                ${k('el)}
              }

            }

            def hasNext(st: St): Expr[Boolean] = {
              val (flag, _, _) = st
              flag.get
            }
          }
        }
      }
    }

    private def pushLinear[A, B, C](producer: Producer[A], nestedProducer: Producer[B], nestedf: (B => StagedStream[C])): StagedStream[(A, C)] = {
      val newProducer = new Producer[(Var[Boolean], producer.St, B)] {

        type St = (Var[Boolean], producer.St, nestedProducer.St)
        val card: Cardinality = Many

        def init(k: St => Expr[Unit]): Expr[Unit] = {
          producer.init(s1 => '{ ${nestedProducer.init(s2 =>
            Var(producer.hasNext(s1)) { flag =>
              k((flag, s1, s2))
            })}})
        }

        def step(st: St, k: ((Var[Boolean], producer.St, B)) => Expr[Unit]): Expr[Unit] = {
          val (flag, s1, s2) = st
          nestedProducer.step(s2, b => '{ ${k((flag, s1, b))} })
        }

        def hasNext(st: St): Expr[Boolean] = {
          val (flag, s1, s2) = st
          '{ ${flag.get} && ${nestedProducer.hasNext(s2)} }
        }
      }

      Nested(newProducer, (t: (Var[Boolean], producer.St, B)) => {
        val (flag, s1, b) = t

        mapRaw[C, (A, C)]((c => k => '{
          ${producer.step(s1, a => '{ ${k((a, c))} })}
          ${flag.update(producer.hasNext(s1))}
        }), addTerminationCondition((b_flag: Expr[Boolean]) => '{ ${flag.get} && $b_flag }, nestedf(b)))
      })
    }

    /** Computes the producer of zipping two linear streams **/
    private def zip_producer[A, B](producer1: Producer[A], producer2: Producer[B]) = {
      new Producer[(A, B)] {

        type St = (producer1.St, producer2.St)
        val card: Cardinality = Many

        def init(k: St => Expr[Unit]): Expr[Unit] = {
          producer1.init(s1 => producer2.init(s2 => k((s1, s2)) ))
        }

        def step(st: St, k: ((A, B)) => Expr[Unit]): Expr[Unit] = {
          val (s1, s2) = st
          producer1.step(s1, el1 => producer2.step(s2, el2 => k((el1, el2)) ))
        }

        def hasNext(st: St): Expr[Boolean] = {
          val (s1, s2) = st
          '{ ${producer1.hasNext(s1)} && ${producer2.hasNext(s2)} }
        }
      }
    }

    /** zip **/
    def zip[B: Type, C: Type](f: (Expr[A] => Expr[B] => Expr[C]), stream2: Stream[B]): Stream[C] = {
      val Stream(stream_b) = stream2
      Stream(mapRaw[(Expr[A], Expr[B]), Expr[C]]((t => k => k(f(t._1)(t._2))), zipRaw[A, Expr[B]](stream, stream_b)))
    }
  }

  object Stream {
    def of[A: Type](arr: Expr[Array[A]]): Stream[A] = {
      val prod = new Producer[Expr[A]] {
        type St = (Var[Int], Var[Int], Expr[Array[A]])

        val card = Many

        def init(k: St => Expr[Unit]): Expr[Unit] = {
          Var('{($arr).length}) { n =>
            Var(0){ i =>
              k((i, n, arr))
            }
          }
        }

        def step(st: St, k: (Expr[A] => Expr[Unit])): Expr[Unit] = {
          val (i, _, arr) = st
          '{
              val el = ($arr).apply(${i.get})
              ${i.update('{ ${i.get} + 1 })}
              ${k('el)}
          }
        }

        def hasNext(st: St): Expr[Boolean] =  {
          val (i, n, _) = st
          '{
              (${i.get} < ${n.get})
          }
        }
      }

      Stream(Linear(prod))
    }
  }

  def test1() = Stream
    .of('{Array(1, 2, 3)})
    .fold('{0}, ((a: Expr[Int], b : Expr[Int]) => '{ $a + $b }))

  def test2() = Stream
    .of('{Array(1, 2, 3)})
    .map((a: Expr[Int]) => '{ $a * 2 })
    .fold('{0}, ((a: Expr[Int], b : Expr[Int]) => '{ $a + $b }))

  def test3() = Stream
    .of('{Array(1, 2, 3)})
    .flatMap((d: Expr[Int]) => Stream.of('{Array(1, 2, 3)}).map((dp: Expr[Int]) => '{ $d * $dp }))
    .fold('{0}, ((a: Expr[Int], b : Expr[Int]) => '{ $a + $b }))

  def test4() = Stream
    .of('{Array(1, 2, 3)})
    .filter((d: Expr[Int]) => '{ $d % 2 == 0 })
    .fold('{0}, ((a: Expr[Int], b : Expr[Int]) => '{ $a + $b }))

  def test5() = Stream
    .of('{Array(1, 2, 3)})
    .take('{2})
    .fold('{0}, ((a: Expr[Int], b : Expr[Int]) => '{ $a + $b }))

  def test6() = Stream
    .of('{Array(1, 1, 1)})
    .flatMap((d: Expr[Int]) => Stream.of('{Array(1, 2, 3)}).take('{2}))
    .take('{5})
    .fold('{0}, ((a: Expr[Int], b : Expr[Int]) => '{ $a + $b }))

  def test7() = Stream
    .of('{Array(1, 2, 3)})
    .zip(((a : Expr[Int]) => (b : Expr[Int]) => '{ $a + $b }), Stream.of('{Array(1, 2, 3)}))
    .fold('{0}, ((a: Expr[Int], b : Expr[Int]) => '{ $a + $b }))

  def test8() = Stream
    .of('{Array(1, 2, 3)})
    .zip(((a : Expr[Int]) => (b : Expr[Int]) => '{ $a + $b }), Stream.of('{Array(1, 2, 3)}).flatMap((d: Expr[Int]) => Stream.of('{Array(1, 2, 3)}).map((dp: Expr[Int]) => '{ $d + $dp })))
    .fold('{0}, ((a: Expr[Int], b : Expr[Int]) => '{ $a + $b }))

  def test9() = Stream
    .of('{Array(1, 2, 3)}).flatMap((d: Expr[Int]) => Stream.of('{Array(1, 2, 3)}).map((dp: Expr[Int]) => '{ $d + $dp }))
    .zip(((a : Expr[Int]) => (b : Expr[Int]) => '{ $a + $b }), Stream.of('{Array(1, 2, 3)}) )
    .fold('{0}, ((a: Expr[Int], b : Expr[Int]) => '{ $a + $b }))

  def test10() = Stream
    .of('{Array(1, 2, 3)}).flatMap((d: Expr[Int]) => Stream.of('{Array(1, 2, 3)}).map((dp: Expr[Int]) => '{ $d + $dp }))
    .zip(((a : Expr[Int]) => (b : Expr[Int]) => '{ $a + $b }), Stream.of('{Array(1, 2, 3)}).flatMap((d: Expr[Int]) => Stream.of('{Array(1, 2, 3)}).map((dp: Expr[Int]) => '{ $d + $dp })) )
    .fold('{0}, ((a: Expr[Int], b : Expr[Int]) => '{ $a + $b }))

  def main(args: Array[String]): Unit = {
    implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)
    println(run(test1()))
    println
    println(run(test2()))
    println
    println(run(test3()))
    println
    println(run(test4()))
    println
    println(run(test5()))
    println
    println(run(test6()))
    println
    println(run(test7()))
    println
    println(run(test8()))
    println
    println(run(test9()))
    println
    println(run(test10()))
  }
}



