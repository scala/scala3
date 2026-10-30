package pkgbase;

public abstract class Base<T> {
  protected interface Parent {}
  protected class Inner {}

  public abstract static class Builder<B> {
    protected Builder(Parent parent) {}
  }
}
