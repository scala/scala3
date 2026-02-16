
object Scaffold {

  trait Arrow
  object Arrow {
    trait Outbound extends Arrow
  }

  trait NodeKOrGraphK {}

  trait NodeK extends NodeKOrGraphK {

    type FBound <: Induction

    protected def getInduction: Seq[FBound]
  }

  trait Induction {
    def arrow: Arrow
    def node: NodeK
  }

  object Induction {

    trait FP[+N <: NodeK] extends Induction { // short for "fixed point"
      def node: N
    }
  }

  trait GraphK extends NodeKOrGraphK {

    type Batch[+T] <: Iterable[T]

    type _Node <: NodeK

    def entries: Batch[_Node]
  }

  trait Topology {

    type FP = Induction.FP[Node]
    type FBound <: FP

    type Node = NodeK { type FBound <: Topology.this.FBound }
    trait Node_ extends NodeK {
      type FBound = Topology.this.FBound
    }

    type Graph = GraphK { type _Node <: Node }
  }

}
