trait Nat {
  def toInt: Int
}

case object Z extends Nat {
  transparent def toInt = 0
}

case class S[N <: Nat](n: N) extends Nat {
  transparent def toInt = n.toInt + 1
}

object Test extends App {
  type Z = Z.type

  type IntOrString(x: Boolean) = if x then Int else String

  val x: IntOrString(true) = 1
  val x1: Int = x
  val y: IntOrString(false) = ""
  val y1: String = y

  type ToNat(n: Int) <: Nat =
    if n == 0 then Z
    else S[ToNat(n - 1)]

  val n0: ToNat(0) = Z
  val n1: ToNat(1) = S(Z)
  val n3: ToNat(3) = S(S(S(Z)))
  val i0: 0 = n0.toInt
  val i1: 1 = n1.toInt
  val i3: 3 = n3.toInt

  def ii: Int = 2
  def nn: ToNat(ii) = S(S(Z)) // no expansion possible, since `ii` is of type `Int`.
  val ii1: Int = nn.toInt

  type nth[F[_], T](n: Int) =
    if n == 0 then T
    else F[nth[F, T](n - 1)]

  val nth0: nth[List, Int](0) = 2
  val nth1: nth[Option, String](1) = Some("hi")
  val nth3: nth[Seq, Boolean](3) = Seq(Seq(Seq(true, false)))
  val nth0a: Int = nth0
  val nth1a: Option[String] = nth1
  val nth3a: Seq[Seq[Seq[Boolean]]] = nth3
}