public @interface MyJava_1 {

    public String value() default "MyJava";

    public MyClassTypeA typeA();

    public MyClassTypeB typeB() default @MyClassTypeB;

    public enum MyClassTypeA {
	A, B
    }

    public @interface MyClassTypeB {}
}

