class A(a: Any) extends annotation.StaticAnnotation
@A({val x = 0}) trait B // error: expression cannot be used inside an annotation argument
