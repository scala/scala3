
public class Foo {
    public static String foo(Bar<? extends Foo> bar) {
        return bar.get.string
    }

    public String string() {
        return "42";
    }
}