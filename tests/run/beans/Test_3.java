import java.util.Arrays;

class JavaTest {
  public A run() throws ReflectiveOperationException{
    A a = new A();
    System.out.println(a.getX());
    System.out.println(a.isY());

    System.out.println(Arrays.asList(a.getClass().getMethod("getRetainingAnnotation").getAnnotations()));

    System.out.println(a.getMutableOneWithLongName());
    a.setMutableOneWithLongName("other text");
    return a;
  }
}