
trait Nat
case object Z extends Nat
case class S[N <: Nat](pred: N) extends Nat

type Z = Z.type
given zero: Z = Z
given succ: [N <: Nat] => (n: N) => S[N] = S(n)

case class Sum[N <: Nat, M <: Nat, R <: Nat](result: R)

given sumZ: [N <: Nat] => (n: N) => Sum[Z, N, N] = Sum(n)
given sumS: [N <: Nat, M <: Nat, R <: Nat]
	=> (sum: Sum[N, M, R])
  => Sum[S[N], M, S[R]]
	= Sum(S(sum.result))

def add[N <: Nat, M <: Nat, R <: Nat](n: N, m: M)(
	using sum: Sum[N, M, R]
): R = sum.result

case class Prod[N <: Nat, M <: Nat, R <: Nat](result: R)


@main def Test: Unit =

	val n1: S[Z] = add(Z, S(Z))
	summon[n1.type <:< S[Z]] // OK

	val n3: S[S[S[Z]]] = add(S(S(Z)), S(Z))
	summon[n3.type <:< S[S[S[Z]]]] // Ok

	val m3_2 = add(S(Z), S(S(Z)))
	summon[m3_2.type <:< S[S[S[Z]]]] // Error before changes: Cannot prove that (m3_2 : S[S[Nat]]) <:< S[S[S[Z]]]

	val m4_2 = add(S(Z), S(S(S(Z))))
	summon[m4_2.type <:< S[S[S[S[Z]]]]]


