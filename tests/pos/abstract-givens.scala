class C { type T }
trait A with
  given C with {}   // concrete
  given c0: C {}    // concrete
  given c1: C {}    // concrete
  given c2: (C {type T <: Int})  // abstract with refinement

object B extends A with
  override given c2: C with { type T = Int }
