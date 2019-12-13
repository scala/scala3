package com.javacp;

import javax.naming.Context;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Hashtable;

public final class Test<A extends CharSequence & Serializable, B> extends ArrayList<A> implements Comparable<B>, Serializable {

    @Override
    public final int compareTo(B b) {
        return 0;
    }

    public void wildcard(ArrayList<?> wildcard) {}
    public void wildcard(Collection<? extends Number> a, Collection<? super Number> b) {}

    class InnerShadowTypeParam<A> {
        public void move(A a, B b) { }
    }

    ArrayList<A> genericField;
    final int[] arrayField = null;

    ArrayList<A> genericMethod() {
        return null;
    }
    int[] arrayMethod() {
        return new int[0];
    }
    void genericParams(A a, B b) { }
    void primitiveParams(int a, long b, float c, double d, short e, byte f, boolean g, char h) { }
    void typeParams(ArrayList<HashMap<A, String[]>> a, Hashtable<String, B> b) { }
    <C extends Integer> void methodTypeParams(C c) { }

    public void overload(java.util.logging.Logger a) { }
    public void overload(Logger a) { }

    // primitive fields
    public int Int;
    public long Long;
    public float Float;
    public short Short;
    public byte Byte;
    public boolean Boolean;
    public char Char;


    private int privateField;
    protected int protectedField;
    public int publicField;
    int packagePrivateField;

    private void privateMethod() { }
    protected void protectedMethod() { }
    public void publicMethod() { }
    void packagePrivateMethod() { }

    public Serializable anonymous = new Serializable() { };

    static int staticField;
    static void staticMethod() {}
    static class StaticClass {}

    void vararg(int a, String... args) {}

    strictfp void strictfpMethod() {}

    <T extends X> void m1() {}
    <T extends X & Y> void m2() {}
    <T extends X & Y & Z> void m3() {}
}

interface X {
}

interface Y {
}

interface Z {
}
