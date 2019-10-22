package mytests.annot;

public class J_1 {

    @TestNotNull
    public static final String s = "s";

    @TestNotNull()
    public static String m(int i) {
        return "m: " + i;
    }

    @TestNotNull
    public static String n(int i) {
        return "n: " + i;
    }
}

@interface TestNotNull {
    int value() default 1;
}
