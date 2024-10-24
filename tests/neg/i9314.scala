final class fooAnnot[T](member: T) extends scala.annotation.StaticAnnotation // must have type parameter

@fooAnnot(new RecAnnotated {}) // error: not a valid annotation
trait RecAnnotated
