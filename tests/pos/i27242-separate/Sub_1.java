package pkgsub;

abstract class Sub extends pkgbase.Base<Sub> {
  public static final class Builder extends pkgbase.Base.Builder<Builder> {
    private Builder(pkgbase.Base.Parent parent) {
      super(parent);
    }
  }

  static final class Helper {
    pkgbase.Base<Sub>.Inner inner(pkgbase.Base<Sub>.Inner inner) { return inner; }

    static final class Nested {
      void parent(pkgbase.Base.Parent parent) {}
    }
  }
}
