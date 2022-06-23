// scalajs: --skip

public class Test {
    public static void main(String[] args) {
        CompatVargs c = new CompatVargs();
        c.vargs("single");
        c.vargs("a", "b");
        c.vargs(new String[]{"a", "b"});
        c.vargsFromScala();
    }
}
