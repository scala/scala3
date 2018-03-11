trait A
trait B

class Test1 {
	def foo(x: List[A]): Function1[A, A] = ???
	def foo(x: List[B]): Function2[B, B, B] = ???
	// ok, different jvm signature
}


class Test2 {
	def foo(x: List[A]): Function1[A, A] = ???
	def foo(x: List[B]): Function1[B, B] = ??? // error: same jvm signature
	// scalac calls this "have same type after erasure"
}


class Test3 {
	// overload with same argument type, but different return types
	def foo(x: List[A]): Function1[A, A] = ???
	def foo(x: List[A]): Function2[B, B, B] = ??? // error
}
