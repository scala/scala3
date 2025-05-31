
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
  def liftAsTupledInThreads[A <: Tuple](f: A => Unit)(using e: Executor): UpdatableMap[A] => Unit = _ => ()
  // eliminate eta-expansion where the partial application returns a context function
  def xliftAsTupledInThreads[A <: Tuple](f: A => Unit): Executor ?=> UpdatableMap[A] => Unit = _ => ()

  run:
    tupledSequence(liftAsTupledInThreads(tupledFunction.tupled)) // error

  run:
    val lifted = liftAsTupledInThreads(tupledFunction.tupled)
    // the expected type induces the symptom
    //val lifted: ((Updatable[Int], Updatable[Int])) => Unit = liftAsTupledInThreads(tupledFunction.tupled)
    tupledSequence(lifted)
