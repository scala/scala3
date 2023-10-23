type A = String
type B = List[A]
type C[X] = List[X]

opaque type OA = String
object OA:
  def test = println(dealiasKeepOpaques[OA])

opaque type OB = List[A]
object OB:
  def test = println(dealiasKeepOpaques[OB])

opaque type OC[X] = List[X]
object OC:
  def test = println(dealiasKeepOpaques[OC[Int]])


@main def Test: Unit = {
  println(dealiasKeepOpaques[String])
  println(dealiasKeepOpaques[A])
  println(dealiasKeepOpaques[B])
  println(dealiasKeepOpaques[C[Int]])
  println(dealiasKeepOpaques[OA])
  println(dealiasKeepOpaques[OB])
  println(dealiasKeepOpaques[OC[Int]])
  OA.test
  OB.test
  OC.test
}
