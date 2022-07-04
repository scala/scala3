// scalajs: --skip

public class Test {
  public static void log(String x) { System.out.println(x); }
  public static void check(Boolean p, String message) {
    if (!p) throw new RuntimeException("Assertion failed: " + message);
  }

  public static <E extends java.lang.Enum<E>> void apiTest(E t1, E t2) {
    log("compareTo greater:" + t1.compareTo(t2)       );
    log("compareTo lesser: " + t2.compareTo(t1)       );
    log("compareTo self:   " + t1.compareTo(t1)       );
    log("equals other:     " + t1.equals(t2)          );
    log("equals self:      " + t1.equals(t1)          );
    log("getDeclaringClass:" + t1.getDeclaringClass() );
    log("name:             " + t1.name()              );
    log("ordinal:          " + t1.ordinal()           );
    log("toString:         " + t1.toString()          );
  }

  public static void moduleTest() {
    A[] values = A.values();
    log("Values class: " + values.getClass());
    for (A v: values) log(v.name() + " : " + v.ordinal());
    log("By-name value: " + A.valueOf("MONDAY"));
    try {
      A.valueOf("stuff");
    }
    catch (IllegalArgumentException e) {
      log("Correctly failed to retrieve illegal name, message: " + e.getMessage());
    }
  }

  public static void collectionsTest() {
    java.util.EnumMap<A, String> days = new java.util.EnumMap<A, String>(A.class);
    days.put(A.MONDAY, "workday");
    days.put(A.SATURDAY, "weekend");

    log("Retrieving Monday: " + days.get(A.MONDAY));
    log("Contains Tuesday: " + days.containsKey(A.TUESDAY));

    java.util.EnumSet<A> allDays = java.util.EnumSet.allOf(A.class);
    log("All days:");
    for (A d : allDays) log(d.toString());
  }

  public static void switchTest() {
    A a = A.TUESDAY;
    switch(a) {
      case MONDAY:
        log("Saw monday");
        break;
      case TUESDAY:
        log("Saw tuesday");
        break;
    }

    B b = B.JUPITER;
    switch (b) {
      case JUPITER: log("Jup"); break;
      case EARTH: log("Earth"); break;
    }
  }

  public static void miscTests() {
    log("Gravity on Earth: " + B.EARTH.gravity());
    check(A.class.isEnum(), "A class must be enum");
    check(B.class.isEnum(), "B class must be enum");
  }

  public static void main(String[] args) {
    log("API Test A");
    apiTest(A.MONDAY, A.TUESDAY);

    log("\nAPI Test B");
    apiTest(B.EARTH, B.JUPITER);

    log("\nModule Test");
    moduleTest();

    log("\nCollections Test");
    collectionsTest();

    log("\nSwitch Test");
    switchTest();

    log("\nMisc Tests");
    miscTests();
  }
}
