package com.javacp;

public class ClassSuffix {
    class Inner<A> {
        class Bar {
            class Fuz<B> {}
        }
    }
    public ClassSuffix.Inner<String>.Bar.Fuz<Integer> suffix;
}
