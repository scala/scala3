package scala.tasty

trait Universe {
  implicit val tasty: Tasty
  implicit val context: tasty.Context
}

object Universe {
  /** Compiler context available in a ~ at inline site */
  implicit def compilationUniverse: Universe = throw new Exception("Not in inline macro.")
}
