import scala.compiletime.testing.typeChecks
import scala.compiletime.error

inline def assertDoesNotCompile(inline code: String): Unit = {
  if (typeChecks(code)) {
    error("Type-checking succeeded unexpectedly.")
  } else {
  }
}

val test1 = assertDoesNotCompile("1") // error
val test2 = assertDoesNotCompile("1.noSuchMethod")
