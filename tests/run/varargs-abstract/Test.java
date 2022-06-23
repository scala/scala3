// scalajs: --skip

import java.util.Comparator;

public class Test {
  public static void main(String[] args) {
    VarargImplClass c = new VarargImplClass();

    c.x(0, 1, 2);
    c.y("a", "b", "c");
    c.z("a", "b", "c");
    c.generic("a", "b", "c");
    c.genericBounded("a", "b", "c");

    VarargAbstractClass<String> i = new VarargImplClass();

    i.x(0, 1, 2);
    i.y("a", "b", "c");
    i.z("a", "b", "c");
    i.generic("a", "b", "c");
    i.genericBounded("a", "b", "c");

    VarargClassBounded<String> b = new VarargClassBounded<>();
    b.v1("a", "b", "c");
    b.v2("a", "b", "c");
  }
}
