import scala.compiletime.{error, codeOf}
import scala.compiletime.testing.*

inline def testError(inline typeName: Any): String = error("Got error " + codeOf(typeName))

transparent inline def compileErrors(inline code: String): List[Error] = typeCheckErrors(code)

def test =
  typeCheckErrors("""testError("string")""")
  compileErrors("""testError("string")""")
