import java.io.File
import java.io.FileFilter
import java.lang.reflect.InvocationTargetException
import java.lang.reflect.Method
import java.lang.reflect.Type

object Test {
  def main(args: Array[String]): Unit = {
    val classLoader = Test.getClass.getClassLoader();

    try {
      val mainClass = classLoader.loadClass("DCETest");
      val mainMethod = mainClass.getMethod("dceTest");
      mainMethod.invoke(null);
    } catch {
      case e: java.lang.Exception => e.getCause.printStackTrace();
    }
  }

  @scala.annotation.internal.DoNotDCE
  def shouldDCE(expr: => Any): Unit = try {
    expr
    throw new Exception("Expected DCE")
  } catch {
    case dce: dotty.runtime.DeadCodeEliminated =>
    // TODO: check stack trace to see if the DCE was in the fist call of expr
  }
}
