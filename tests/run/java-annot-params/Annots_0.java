package annots;

import java.lang.annotation.*;

@Retention(RetentionPolicy.RUNTIME)
@interface WithClass {
  Class<?> arg();
}

@Retention(RetentionPolicy.RUNTIME)
@interface WithClassDefaultName {
  Class<?> value();
}

@Retention(RetentionPolicy.RUNTIME)
@interface WithString {
  String arg();
}

@Retention(RetentionPolicy.RUNTIME)
@interface WithReference {
  String arg();
}

@Retention(RetentionPolicy.RUNTIME)
@interface WithBoolean {
  boolean arg();
}

@Retention(RetentionPolicy.RUNTIME)
@interface WithFloat {
  float arg();
}

@Retention(RetentionPolicy.RUNTIME)
@interface WithNested {
  Nested arg();
}

@Retention(RetentionPolicy.RUNTIME)
@interface Nested {
  String value();
}

@Retention(RetentionPolicy.RUNTIME)
@interface WithArray {
  String[] value();
}

@Retention(RetentionPolicy.RUNTIME)
@interface WithEmptyArray {
  String[] value();
}

@Retention(RetentionPolicy.RUNTIME)
@interface WithSingleElement {
  String[] value();
}

@Retention(RetentionPolicy.RUNTIME)
@interface WithMultipleArgs {
  int[] ints();
  float floatVal();
  Nested[] annots();
  Class<?> clazz();
  String[] value();
}

@Retention(RetentionPolicy.RUNTIME)
@interface ShouldNotCrash {
  String value();
}

@Retention(RetentionPolicy.RUNTIME)
@interface ShouldAlsoNotCrash {
  String value();
  int[] ints();
}

class A {
  static final String CONST = "VALUE OF CONST";
}
