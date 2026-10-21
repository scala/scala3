package genericouterinnermember;

public class Use_2 {
    public static String use(Outer<String> outer) {
        return outer.inner().use("ok");
    }
    public static String use2(Outer<String> outer) {
        return outer.inner2().use("ok");
    }
}
