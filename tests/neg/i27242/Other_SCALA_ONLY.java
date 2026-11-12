package pkgsub;

public class Other {
  static final class Nested {
    void parent(pkgbase.Base.Parent parent) {} // error
  }
}
