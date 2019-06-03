trait A
trait B

// test with classes

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

class Test4 {
	val foo = 1
	def foo = 2 // error
}

class Test4b {
	def foo = 2
	val foo = 1 // error
}

class Test4c {
	def foo = 2
	var foo = 1 // error
}

class Test4d {
	var foo = 1
	def foo = 2 // error
}


// test with traits

trait Test5 {
	def foo(x: List[A]): Function1[A, A] = ???
	def foo(x: List[B]): Function2[B, B, B] = ???
	// ok, different jvm signature
}

trait Test6 {
	def foo(x: List[A]): Function1[A, A] = ???
	def foo(x: List[B]): Function1[B, B] = ??? // error: same jvm signature
	// scalac calls this "have same type after erasure"
}

trait Test7 {
	// overload with same argument type, but different return types
	def foo(x: List[A]): Function1[A, A] = ???
	def foo(x: List[A]): Function2[B, B, B] = ??? // error
}

class Test8 {
	val foo = 1
	def foo = 2 // error
}

class Test8b {
	def foo = 2
	val foo = 1 // error
}

class Test8c {
	def foo = 2
	var foo = 1 // error
}

class Test8d {
	var foo = 1
	def foo = 2 // error
}

// test method and constructor argument clashing

class Test9(val foo: Int) {
	def foo: String // error
}

class Test10(val foo: Int) {
	def foo: Int // error
}

abstract class Test11(val foo: Int) {
	def foo: String // error
}

abstract class Test12(val foo: Int) {
	def foo: Int // error
}

class Test13(var foo: Int) {
	def foo: String // error
}

class Test14(var foo: Int) {
	def foo: Int // error
}

abstract class Test15(var foo: Int) {
	def foo: String // error
}

abstract class Test16(var foo: Int) {
	def foo: Int // error
}

// don't error when shadowing

class Test17 {
	val foo = 1
	def bar() = {
		val foo = ""
	}
}

// no error when overloading

class Test18 {
	def foo(a: A) = 1
	def foo(b: B) = 1
}

