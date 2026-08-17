package scala.specialize
import language.experimental.erasedDefinitions
import scala.annotation.nowarn

@nowarn
sealed trait Specialized[T] extends compiletime.Erased

object Specialized:
    /* @nowarn: New anonymous class definition will be duplicated at each inline site, 
                however it's erased at runtime so we don't care. */
    @nowarn inline def apply[T] = new Specialized[T] {}
