class foo(x: Any) extends annotation.StaticAnnotation

@foo(new AnyRef { }) trait A // error: expression cannot be used inside an annotation argument
