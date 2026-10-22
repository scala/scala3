# Missing Java outer class dependency tests

## Regenerate the test files

**broken/Outer.java**
```java
package broken;
public class Outer {
  public static class Inner {}
}
```

**broken/Child.java**
```java
package broken;
public class Child extends Outer.Inner {}
```

```bash
javac -d out broken/Outer.java broken/Child.java
```

Copy `Child.class` and `Outer$Inner.class` to `cp/broken/`, but do not copy
`Outer.class`.

**suggestions/SearchRoot.java**
```java
package suggestions;

public class SearchRoot extends broken.Child {}
```

```bash
javac -classpath out -d search-root-out suggestions/SearchRoot.java
```

Copy `SearchRoot.class` to `cp/suggestions/`.
