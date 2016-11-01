import java.io.File
import java.io.FileFilter
import java.lang.reflect.InvocationTargetException
import java.lang.reflect.Method
import java.lang.reflect.Type

object Test {
	def main(args: Array[String]): Unit = {
		val classLoader = Test.getClass.getClassLoader();

		try {
			val mainClass = classLoader.loadClass("Main");
			val mainMethod = mainClass.getMethod("testEntry");
			mainMethod.invoke(null);
		} catch {
			case e: java.lang.Exception => e.getCause.printStackTrace();
		}
	}
}
