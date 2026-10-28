import java.lang.invoke.{MethodHandles, VarHandle}
import scala.annotation.static
import scala.reflect.{ClassTag, classTag}

class Wrapper(private val underlying: Int)
object Wrapper:
  @static private val vh = Helper.createVH[Wrapper, Int]("underlying")
object Helper:
  inline def createVH[Wrapper: ClassTag, Var: ClassTag](name: String): VarHandle =
    MethodHandles.lookup().findVarHandle(
      classTag[Wrapper].runtimeClass,
      name,
      classTag[Var].runtimeClass,
    )
