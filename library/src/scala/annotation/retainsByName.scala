package scala.annotation

/** An annotation that indicates capture of an enclosing by-name type
 */
@experimental class retainsByName[Args](xs: (Any@retainsArg)*) extends annotation.StaticAnnotation

