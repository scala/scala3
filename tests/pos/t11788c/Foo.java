package p;

public class Foo {
	private String java;

	// java is class in scope, not the term member or package
	public java.lang.Integer.Inner test() {
		throw null;
	}
}
