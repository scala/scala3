
trait Schema[T]
trait SchemaType[T] extends Schema[T]

object Schema {
  given intSchema: SchemaType[Int] = new SchemaType { override def toString = "INT" }
  given byteSchema: SchemaType[Byte] = new SchemaType { override def toString = "BYTE" }
  given optionSchema[T](using t: Schema[T]): Schema[Option[T]] = new Schema { override def toString = s"OPTION[$t]" }
  given mapSchema[K, V](using k: Schema[K], v: Schema[V]): Schema[Map[K, V]] = new Schema { override def toString = s"MAP[$k,$v]" }
}

/** Intereter for Schema.
 *  Assumes that all instances of schema come from `object Schema`
 */
object SchemaInterpreter {
  import scala.quoted.*

  def interpretSchemaType[T: Type](schema: Expr[SchemaType[T]])(using Quotes): Option[SchemaType[T]] =
    schema match {
      case '{ Schema.intSchema } => Some(Schema.intSchema.asInstanceOf[SchemaType[T]])
      case '{ Schema.byteSchema } => Some(Schema.byteSchema.asInstanceOf[SchemaType[T]])
      case _ => None
    }

  def interpretSchema[T: Type](schemaExpr: Expr[Schema[T]])(using Quotes): Option[Schema[T]] =
    schemaExpr match {
      case '{ $typeSchemaExpr: SchemaType[T] } =>
        interpretSchemaType(typeSchemaExpr)
      case '{ Schema.optionSchema[t](using $tSchemaExpr) } =>
        for tSchema <- interpretSchema(tSchemaExpr)
        yield Schema.optionSchema(using tSchema).asInstanceOf[Schema[T]]
      case '{ Schema.mapSchema[k, v](using $kSchemaExpr, $vSchemaExpr) } =>
        for kSchema <- interpretSchema(kSchemaExpr)
            vSchema <- interpretSchema(vSchemaExpr)
        yield Schema.mapSchema(using kSchema, vSchema).asInstanceOf[Schema[T]]
      case _ =>
        None // could also hangle with `quotes.reflect.{error, throwError}`
    }
}

object Macro {
  import scala.quoted.*

  inline def useSchema[T](using inline schema: Schema[T]): String =
    ${ useSchemaExpr[T]('schema) }

  private def useSchemaExpr[T: Type](schemaExpr: Expr[Schema[T]])(using Quotes): Expr[String] = {
    val schemaOpt: Option[Schema[T]] = SchemaInterpreter.interpretSchema(schemaExpr)
    Expr(schemaOpt.toString)
  }
}
