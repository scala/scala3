final class fooAnnot[T](member: T) extends scala.annotation.StaticAnnotation

@fooAnnot(new RecAnnotated {}) // error: expression cannot be used inside an annotation argument
trait RecAnnotated
