class AnnoJavaParent {
    public String n(int i) {
        return "n: " + i;
    }
}

public class AnnoJava extends AnnoJavaParent {

    @MyAnno
    int f() {
        return 0;
    }

    @MyAnno(1)
    int g() {
        return 1;
    }

    @MyAnno()
    public static String m(int i) {
        return "m: " + i;
    }

    @Override
    public String n(int i) {
        return "n: " + i;
    }
}

@interface MyAnno {
    int value() default 1;
}

