package a;

public class InnerClassGen<T> {

  public class Inner<U> {
    public T rootField;
    public U innerField;

    public Inner(T rootField, U innerField) {
      this.rootField = rootField;
      this.innerField = innerField;
    }

    public T getRootField() {
      return rootField;
    }

    public U getInnerField() {
      return innerField;
    }
  }

  public class Outer<U> {

    public class Nested<V> {
      public T rootField;
      public U outerField;
      public V innerField;

      public Nested(T rootField, U outerField, V innerField) {
        this.rootField = rootField;
        this.outerField = outerField;
        this.innerField = innerField;
      }

      public T getRootField() {
        return rootField;
      }

      public U getOuterField() {
        return outerField;
      }

      public V getInnerField() {
        return innerField;
      }
    }
  }

  public static class OuterStatic<U> {
    public static class NestedStatic<V> {
    }
  }

  public <U> Inner<U> createInner(T rootField, U innerField) {
    return new Inner<>(rootField, innerField);
  }

  public <U, V> Outer<U>.Nested<V> createNested(T rootField, U outerField, V innerField) {
    Outer<U> outer = new Outer<>();
    return outer.new Nested<>(rootField, outerField, innerField);
  }

  public static <T, U> InnerClassGen<T>.Inner<U> createInnerStatic(T rootField, U innerField) {
    InnerClassGen<T> innerClassGen = new InnerClassGen<>();
    return innerClassGen.new Inner<>(rootField, innerField);
  }

  public static <T, U, V> InnerClassGen<T>.Outer<U>.Nested<V> createNestedStatic(T rootField, U outerField, V innerField) {
    InnerClassGen<T> innerClassGen = new InnerClassGen<>();
    InnerClassGen<T>.Outer<U> outer = innerClassGen.new Outer<>();
    return outer.new Nested<>(rootField, outerField, innerField);
  }

  public static <T, U, V> void consumeNestedStatic(InnerClassGen<T>.Outer<U>.Nested<V> nested) {
  }

}
