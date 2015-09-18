    trait Periods { self: Context =>

      def atPhase(id: Int): Unit = println(s"atPhase: $id")

    }

    class Phase(val id: Int)

    trait Phases { self: Context =>
      def atPhase(phase: Phase): Unit = self.atPhase(phase.id)
    }

    trait Context extends Phases with Periods

    object Test extends Context {

      def main(args: Array[String]) = atPhase(new Phase(2))

    }

