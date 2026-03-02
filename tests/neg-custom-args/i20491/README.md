# Test case for https://github.com/scala/scala3/issues/20491

## Test setup

The test simulates a missing class dependency:

- `cp/testpkg/A.class` + `cp/testpkg/A.tasty` - A Scala trait that references `otherpkg.MissingDep`
- `cp/otherpkg/` - An empty directory (`MissingDep.class` is missing)

## How to regenerate the test files

**otherpkg/MissingDep.java**
```java
package otherpkg;
public class MissingDep {}
```

**A.scala**
```scala
package testpkg
trait A:
  def getDep: otherpkg.MissingDep
```

```bash
scala-cli compile A.scala otherpkg/MissingDep.java -d out
```

Copy `A.class` and `A.tasty` to `cp/testpkg/`, and ensure `cp/otherpkg/` directory
exists but is empty (do not include `MissingDep.class`).
