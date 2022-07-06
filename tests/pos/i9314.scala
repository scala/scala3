final class fooAnnot[T](member: T) extends scala.annotation.StaticAnnotation // must have type parameter

@fooAnnot(new RecAnnotated {}) // must pass instance of anonymous subclass
trait RecAnnotated
