//> using options -source:future

/** Why the singletonInterval logic cannot be applied for lubArgs and glbArgs in TypeComparer. */

type Or[+A, +B] = A | B

object TestGlb:
  val x: Or["3", Singleton] & Or[Singleton, "3"] = 3
  val y: Or["3", "3"] = x // error
  val z: String = y

object TestLub:
  def f[P[_, _]](x1: P["3", Singleton], x2: P[Singleton, "3"]): P["3", "3"] =
    val x = if true then x1 else x2
    x // error, was accepted because inferred type of x was `P["3", "3"]`
    // by going through Types.join, TypeOps.mergeRefinedOrApplied, TypeComparer#lubArgs, TypeComparer#singletonInterval
  val z: String = f[Or](3, 3)
