import java.io.File
import java.io.FileFilter
import java.lang.reflect.InvocationTargetException
import java.lang.reflect.Method
import java.lang.reflect.Type

import dotty.runtime.DeadCodeEliminated

import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 120, classesWithReachableMethods = 14, reachableMethods = 65)
  def main(args: Array[String]): Unit = {
    val classLoader = Test.getClass.getClassLoader()

    try {
      val mainClass = classLoader.loadClass("Test")
      val mainMethod = mainClass.getMethod("dceTest")
      mainMethod.invoke(null);
    } catch {
      case e: java.lang.Exception =>
        if (e.getCause.getMessage.contains("was invoked trough java reflection from"))
          println("OK")
    }
  }

  @internal.link.AssertNotReachable
  @internal.link.DoNotDeadCodeEliminate
  def dceTest: Unit = {
    System.out.println("dceTest")
    throw new DeadCodeEliminated
  }
}
