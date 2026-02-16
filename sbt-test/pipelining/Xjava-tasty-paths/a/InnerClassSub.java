package a;

public class InnerClassSub extends InnerClass {

  public class InnerSub<U> extends Inner<U> {
    public InnerSub(U innerField) {
      super(innerField);
    }
  }

  public class OuterSub<U> extends Outer<U> {
    public OuterSub() {
      super();
    }
  }

  public <U> Inner<U> createInnerSub(U innerField) {
    return new InnerSub<>(innerField);
  }

  public <U, V> Outer<U>.Nested<V> createNestedSub(U outerField, V innerField) {
    OuterSub<U> outer = new OuterSub<>();
    return outer.new Nested<>(outerField, innerField);
  }

  public <U> InnerClass.Inner<U> createInnerSub2(U innerField) {
    return new InnerSub<>(innerField);
  }

  public <U, V> InnerClass.Outer<U>.Nested<V> createNestedSub2(U outerField, V innerField) {
    OuterSub<U> outer = new OuterSub<>();
    return outer.new Nested<>(outerField, innerField);
  }

  public static <U> InnerClass.Inner<U> createInnerStatic(U innerField) {
    InnerClassSub innerClass = new InnerClassSub();
    return innerClass.new Inner<>(innerField);
  }

  public static <U, V> InnerClass.Outer<U>.Nested<V> createNestedStatic(U outerField, V innerField) {
    InnerClassSub innerClass = new InnerClassSub();
    InnerClassSub.Outer<U> outer = innerClass.new Outer<>();
    return outer.new Nested<>(outerField, innerField);
  }

  public static <U, V> void consumeNestedStatic(InnerClass.Outer<U>.Nested<V> nested) {
  }

  public static <U, V> void consumeNestedStatic2(Outer<U>.Nested<V> nested) {
  }

}
