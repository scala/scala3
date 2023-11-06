// this test ensures that it is possible to read a generic java class from TASTy.
package a;

public abstract class A<T> {
  private final int _value;

  protected A(final int value) {
    this._value = value;
  }

  public int value() {
    return _value;
  }
}
