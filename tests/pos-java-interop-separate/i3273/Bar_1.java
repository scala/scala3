class Bar_1 {
	static abstract class A<X> extends B {
		abstract B foo();
	}
	static abstract class B<X> {
		abstract A bar();
	}
}
