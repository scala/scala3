object App {
  type Id[A] >: A <: A
  def v1: Id[_] = ??? // error

  type HkL[A] >: A
  def v2: HkL[_] = ??? // error

  type HkU[A] <: A
  def v3: HkU[_] = ???  // error

  type HkAbs[A]
  def v4: HkAbs[_] = ??? // error
}
