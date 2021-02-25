public abstract class A<T> {
    public abstract void foo(T value);
    public void foo(T value, Object obj) { return; }
}
