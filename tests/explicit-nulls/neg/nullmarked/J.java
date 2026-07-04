package test;

import org.jspecify.annotations.*;

public class J {

    private static String getK() {
        return "k";
    }

    public static final String k = getK();

    public static String l = "l";

    @NullUnmarked
    public static class J2 {
        public static final String k2 = getK();

        public static String l2 = "l";
    }
}
