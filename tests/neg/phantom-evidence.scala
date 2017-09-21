/** In this implementation variant of =:= (called =::=) we erase all instantiations and definitions of =::= */
object WithNormalState extends Phantom {

  type =::=[From, To] <: this.Any

  implicit inline def tpEquals[A]: A =::= A = assume

  trait State
  sealed trait On extends State
  sealed trait Off extends State

  object Instance {
    def newInstance(): Instance[Off] = new Instance[Off]
  }
  class Instance[S <: State] private {
    def getOnInstance(implicit ev: S =::= Off): Instance[On] = new Instance[On]
    def getOffInstance(implicit ev: S =::= On): Instance[Off] = new Instance[Off]
  }

  def run() = {
    val instance = Instance.newInstance()
    instance.getOffInstance // error
    instance.getOnInstance.getOnInstance // error
  }

}

