package pkg;

class Constants_1 {
  // javac will produce a ConstantValue 1 for the annotation argument
  @Annot_1(BOOL=true) static void foo() {};

  // ...and reuse it here for the ConstantValue attribute
  static final byte BYTE  = 1;
}
