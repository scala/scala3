public class B {
	static interface T<X> extends C {}

	// not public, so this shouldn't be tracked as an inherited dependency
	private class Q implements E<Integer> {}

	public void x(int i) {
		// not public, not an inherited dependency
		D j = new D() {};
	}
}
