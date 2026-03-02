// InnerClass.java

package lib;

public class InnerClass {

  public class Inner<U> {
    public U innerField;

    public Inner(U innerField) {
      this.innerField = innerField;
    }

    public U getInnerField() {
      return innerField;
    }
  }

  public class Outer<U> {

    public class Nested<V> {

      public U outerField;
      public V innerField;

      public Nested(U outerField, V innerField) {
        this.outerField = outerField;
        this.innerField = innerField;
      }

      public U getOuterField() {
        return outerField;
      }

      public V getInnerField() {
        return innerField;
      }
    }
  }

  public <U> Inner<U> createInner(U innerField) {
    return new Inner<>(innerField);
  }

  public <U, V> Outer<U>.Nested<V> createNested(U outerField, V innerField) {
    Outer<U> outer = new Outer<>();
    return outer.new Nested<>(outerField, innerField);
  }

  public static <U> InnerClass.Inner<U> createInnerStatic(U innerField) {
    InnerClass innerClass = new InnerClass();
    return innerClass.new Inner<>(innerField);
  }

  public static <U, V> InnerClass.Outer<U>.Nested<V> createNestedStatic(U outerField, V innerField) {
    InnerClass innerClass = new InnerClass();
    InnerClass.Outer<U> outer = innerClass.new Outer<>();
    return outer.new Nested<>(outerField, innerField);
  }

  public static <U, V> void consumeNestedStatic(InnerClass.Outer<U>.Nested<V> nested) {
  }

  public static <U, V> void consumeNestedStatic2(Outer<U>.Nested<V> nested) {
  }

}
