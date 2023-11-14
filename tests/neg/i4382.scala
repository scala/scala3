object App {
  type Id[A] >: A <: A
  def v1: Id[?] = ??? // error

  type HkL[A] >: A
  def v2: HkL[?] = ??? // error

  type HkU[A] <: A
  def v3: HkU[?] = ???  // error

  type HkAbs[A]
  def v4: HkAbs[?] = ??? // error
}
