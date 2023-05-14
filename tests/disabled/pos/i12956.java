// Disable because it requires Java 9+ but our CI still runs on Java 8 and our
// testing infrastructure doesn't let us run a test only on Java 9+.
interface Foo {
  private void bar() {}
}
