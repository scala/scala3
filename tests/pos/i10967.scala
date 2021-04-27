trait SomeTrait

trait CollBase[A <: SomeTrait, +CC1[A2 <: SomeTrait]] {
  val companion: CollCompanion[CC1]
}

trait Coll[A <: SomeTrait] extends CollBase[A, Coll]

trait CollCompanion[+CC2[A <: SomeTrait]]