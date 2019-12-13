package com.javacp;

public class MetacJava {
    public static class StaticInner {
        public void isNotStatic() {}
        public static void isStatic() {}
        public class NonStatic {
            public void method(NonStatic e) {}
        }
    }
    public class Overload1 { public class A {} }
    public static class Overload3 {
        public static class A {}
    }
    public class Overload2 { public class A {} }
    public void overload(Overload1.A a) {}
    // NOTE: Overload3 is intentionally placed before Overload2 in order to test
    // that methods are sorted by whether they're static or not.
    public static void overload(Overload3.A a) {}
    public void overload(Overload2.A a) {}
}
