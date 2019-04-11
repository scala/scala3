/** In this implementation variant of =:= (called =::=) we erase all instantiations and definitions of =::= */
object WithNormalState {
  import Utils._

  trait State
  sealed trait On extends State
  sealed trait Off extends State

  object Instance {
    def newInstance(): Instance[Off] = new Instance[Off]
  }
  class Instance[S <: State] private {
    def getOnInstance given erased (ev: S =::= Off): Instance[On] = new Instance[On] // phantom parameter ev is erased
    def getOffInstance given erased (ev: S =::= On): Instance[Off] = new Instance[Off] // phantom parameter ev is erased
  }

  def run() = {
    val inst = Instance.newInstance()
    inst.getOnInstance // inferred phantom evidence parameter =::= is erased
    inst.getOnInstance.getOffInstance.getOnInstance.getOffInstance // all inferred phantom evidence parameters =::= are erased
  }

}

object Utils {
  type =::=[From, To]
  implicit erased def tpEquals[A]: A =::= A = ???
}
