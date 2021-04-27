object variance {
	enum SUB[-A, +B]:
		case Refl[S]() extends SUB[S, S]
	// Covariant
	class C1[+T](v: T){
		def foo(ev: T SUB Int): Int =
			ev match {
				case SUB.Refl() => v
			}
	}

	// Contravariant
	class B2[-T](v: T){}

	class C2[-T](v: T){
		def foo(ev: Int SUB T): B2[T] =
			ev match {
				case SUB.Refl() => new B2(v)
			}
	}

	// Variance with inheritance

	// superclass covariant and subclass covariant

	class A3[+T](v: T) {
		val value = v
	}

	class C3[+T](v: T) extends A3[T](v){
		def foo(ev: T SUB Int): Int =
			ev match {
				case SUB.Refl() => value
			}
	}


	// superclass covariant and subclass invariant
	class A4[+T](v: T) {
		val value = v
	}

	class C4[T](v: T) extends A4[T](v){
		def foo(ev: T SUB Int): Int =
			ev match {
				case SUB.Refl() => value
			}
	}

	// superclass contravariant and subclass contravariant
	class B5[-T](v: T){}

	class A5[-T](v: T) {
		val value = new B5(v)
	}

	class C5[-T](v: T) extends A5[T](v){
		def foo(ev: Int SUB T): B5[T] =
			ev match {
				case SUB.Refl() => value
			}
	}

	// superclass contravariant and subclass invariant
	class B6[-T](v: T){}

	class A6[-T](v: T) {
		val value = new B6(v)
	}

	class C6[-T](v: T) extends A6[T](v){
		def foo(ev: Int SUB T): B6[T] =
			ev match {
				case SUB.Refl() => value
			}
	}
}