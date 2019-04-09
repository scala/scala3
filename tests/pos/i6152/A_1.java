abstract class A {
  public abstract Object f();

  public static abstract class B extends A {
    @Override
    public abstract String f();
  }
}
