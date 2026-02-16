class TyreCompiler[IN <: Tuple, R](val context: Context[R *: IN]):
  import context.*

  private class Loop[IS <: Tuple, T](
      val context: Context[T *: IS],
      innerAutomaton: context.Automaton[IS]
  ) {
    private lazy val fixableStates: List[RefinedInitNonAcceptingState[T, IS]] =
      innerAutomaton.initStates.map { case is: context.InitNonAcceptingState[?] =>
        new RefinedInitNonAcceptingState[T, IS] {
          type Tail = is.OS
          type OS = List[T] *: Tail
          lazy val state = new NonAcceptingState:
            val next: List[Transition[OS]] =
              is.state.next.flatMap(fixTransition[is.OS](fixableStates, _))
        }
      }

    private def fixTransition[S <: Tuple](
        initStates: List[RefinedInitNonAcceptingState[T, IS]],
        transition: context.Transition[S]
    ): List[Transition[List[T] *: S]] = ???

    private trait RefinedInitNonAcceptingState[T, IS <: Tuple] extends InitNonAcceptingState[IS]:
      type Tail <: Tuple
      type OS = List[T] *: Tail
      lazy val state: NonAcceptingState[OS]
  }

private class Context[R <: Tuple]:
  sealed trait State[S <: Tuple]:
    val next: List[Transition[S]]
  trait NonAcceptingState[S <: Tuple] extends State[S]

  sealed trait Transition[IS <: Tuple]:
    def state: State[?]

  sealed trait InitState[-IS <: Tuple]:
    type OS <: Tuple
    def state: State[?]

  trait InitNonAcceptingState[-IS <: Tuple] extends InitState[IS]:
    lazy val state: NonAcceptingState[OS]

  trait Automaton[-IS <: Tuple]:
    val initStates: List[InitState[IS]]
