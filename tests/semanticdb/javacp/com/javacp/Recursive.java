package com.javacp;

import java.io.Serializable;
import java.util.ArrayList;

public abstract class Recursive<
        A extends Recursive<A, B> & Serializable,
        B extends Recursive.Inner<A, B>> {

    public abstract static class Inner<
            A extends Recursive<A, B> & Serializable,
            B extends Inner<A, B>> {
    }

    public abstract class Inner2<C extends Comparable<C>> {}

    public <Anon> ArrayList<Anon> foo() {
        return new ArrayList<Anon>() {
            @Override
            public boolean remove(Object o) {
                return true;
            }
        };
    }

}
