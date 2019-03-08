class foo(annotation: Any)
@foo(new AnyRef {}) trait A // error: found: foo, required: scala.annotation.Annotation
