object inheritance{
	enum SUB[-A, +B]:
		case Refl[S]() extends SUB[S, S]

	class A[T](val v: T) {
		val foo1: T = v
	}

	class C[T](val v1: T) extends A[T](v1) {
		def eval1(t: T, e: SUB[T, Int]): Int =
			e match {
				case SUB.Refl() => foo1
			}
	}
}