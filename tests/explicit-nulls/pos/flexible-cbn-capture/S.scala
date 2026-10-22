import scala.util.Try

// Minimization of a swagger-akka-http regression. Passing the Java
// `mapper.createProperty().getClass` inline to the generic `readValue` triggers
// capture conversion using a `TypeBox#CAP` skolem. Because the Java `readValue`
// return type is flexible, the by-name argument to `Try.apply` has type
// `FlexibleType(TypeBox[Nothing, (Schema[?])?]#CAP)`.
//
// A `FlexibleType` is now represented as `AppliedType(FlexibleType, hi :: Nil)`,
// so `hasCaptureConversionArg` must look through the flexible wrapper before its
// `AppliedType` case, otherwise it mistakes the wrapped `CAP` for a genuine
// wildcard type argument and rejects the by-name argument with
// "argument for by-name parameter is not a value".
object Test:
  val mapper: J.Mapper = new J.Mapper()

  def tryCorrect(itemSchema: J.Schema[?]): Any =
    Try {
      val primitiveProperty = mapper.createProperty()
      val corrected = mapper.readValue(primitiveProperty.getClass)
      corrected
    }.toOption.getOrElse(itemSchema)
