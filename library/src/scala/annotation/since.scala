package scala.annotation

/** An annotation that is used to mark symbols added to the stdlib after 3.0 release */
private[scala] class since(scalaRelease: String) extends scala.annotation.StaticAnnotation