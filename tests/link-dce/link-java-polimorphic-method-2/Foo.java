
public interface Foo {
    public <T extends Foo> String foo(T t);

    public String string();
}