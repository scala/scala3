import java.util.Comparator;

public class Test {
  public static void main(String[] args) {
    ClassImplementsClass c = new ClassImplementsClass();

    c.x("a", "b", "c");
    c.y("a", "b", "c");
    c.z("a", "b", "c");

    VarargAbstractClass i = new ClassImplementsClass();

    i.x("a", "b", "c");
    i.y("a", "b", "c");
    // i.z("a", "b", "c");
    // ClassCastException at runtime because the generated
    // signature of z doesn't mention the type parameter (it should)
  }
}
