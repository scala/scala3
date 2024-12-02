package p;

public class Foo {
	private String java;

	// java is the rooted package, not the field
	public java.lang.Integer test() {
		//return Integer.valueOf(42);
		throw null;
	}
}
