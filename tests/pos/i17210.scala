
object BugReport:
  trait Executor
  trait Updatable[+A]

  def run(task: Executor ?=> Unit): Unit = ()
  def tupledFunction(a: Int, b: Int): Unit = ()
  def tupledSequence(f: ((Updatable[Int], Updatable[Int])) => Unit): Unit = ()

  type UpdatableMap[T <: Tuple] = T match
    case EmptyTuple => EmptyTuple
    case h *: t => Updatable[h] *: UpdatableMap[t]
  // eliminate the match type
  type xUpdatableMap[_] = (Updatable[Int], Updatable[Int])

  // so that expected type is satisfied, avoid eta-expansion and subsequent error
  transparent inline // presumably allows match type "expansion" before adaptation
  def liftAsTupledInThreads[A <: Tuple](f: A => Unit)(using e: Executor): UpdatableMap[A] => Unit = _ => ()
  // eliminate eta-expansion where the partial application returns a context function
  def xliftAsTupledInThreads[A <: Tuple](f: A => Unit): Executor ?=> UpdatableMap[A] => Unit = _ => ()

  run:
    tupledSequence(liftAsTupledInThreads(tupledFunction.tupled)) // was error

  run:
    val lifted = liftAsTupledInThreads(tupledFunction.tupled)
    // the expected type induces the symptom
    //val lifted: ((Updatable[Int], Updatable[Int])) => Unit = liftAsTupledInThreads(tupledFunction.tupled)
    tupledSequence(lifted)

object BugReport2:
  trait Executor
  trait Updatable[+A]

  def run(task: Executor ?=> Unit): Unit = ()
  def function(a: Int): Unit = ()
  def normalSequence(f: Updatable[Int] => Unit): Unit = ()

  def liftInThreads[A](f: A => Unit)(using e: Executor): Updatable[A] => Unit = _ => ()

  run:
    normalSequence(liftInThreads(function))
