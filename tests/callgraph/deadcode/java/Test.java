import java.io.File;
import java.io.FileFilter;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Type;

import com.sun.org.apache.bcel.internal.util.ClassPath.ClassFile;

public class Test {
	public static void main(String[] args) {
		ClassLoader classLoader = Test.class.getClassLoader();

		try {
			Class mainClass = classLoader.loadClass("Main");
			Method mainMethod = mainClass.getMethod("testEntry");
			mainMethod.invoke(null);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
}