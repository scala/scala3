final class fooAnnot[T](member: T) extends scala.annotation.StaticAnnotation // must have type parameter

@fooAnnot(new RecAnnotated {}) // error: expression cannot be used inside an annotation argument
trait RecAnnotated
