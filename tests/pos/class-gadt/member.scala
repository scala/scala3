object member{
	enum SUB[-A, +B]:
		case Refl[S]() extends SUB[S, S]

	class C[T] {
		def eval1(t: T, e: SUB[T, Int]): Int =
			e match {
				case SUB.Refl() => t + 2
			}
	}
}