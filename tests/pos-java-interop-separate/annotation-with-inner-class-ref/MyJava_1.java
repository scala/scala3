public @interface MyJava_1 {

    public MyClassTypeA typeA() default MyClassTypeA.A;

    public MyClassTypeB typeB();

    public enum MyClassTypeA {
	A
    }

    public @interface MyClassTypeB {}
}

