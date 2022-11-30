public class Test {
    public static void main(String[] args) throws Exception {
        Foo foo = new Foo();
        System.out.println(foo.testNoParam());
        System.out.println(foo.testSingleParam(2));
        System.out.println(foo.testSingleParam2(21).value());
        System.out.println(foo.testSingleParam3(new Box(22)));
        System.out.println(foo.testOtherReturn(3));
        System.out.println(foo.testNoErasure("4"));
        System.out.println(foo.testMultiParam(5, "5"));

        System.out.println(foo.testVCNoParam());
        System.out.println(foo.testVCSingleParam(2));
        System.out.println(foo.testVCOtherReturn(3));
        System.out.println(foo.testVCNoErasure("4"));
        System.out.println(foo.testVCMultiParam(5, "5"));
    }
}
