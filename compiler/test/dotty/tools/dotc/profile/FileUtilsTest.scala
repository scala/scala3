package dotty.tools.dotc.profile

import java.io.*

import org.junit.Assert.*
import org.junit.*

class FileUtilsTest {

  @Test def writeIsSame(): Unit = {
    val fileTest = File.createTempFile("FileUtilsTest", "t1").nn
    val fileExpected = File.createTempFile("FileUtilsTest", "t2").nn

    val sTest = FileUtils.newAsyncBufferedWriter(new FileWriter(fileTest), threadsafe = false)
    val sExpected = new BufferedWriter(new FileWriter(fileExpected))

    def writeBoth(s:String, asChars: Boolean) = {
      if (asChars) {
        sTest.write(s.toCharArray)
        sExpected.write(s.toCharArray)
      } else {
        sTest.write(s)
        sExpected.write(s)
      }
    }

    for (i <- 1 to 2000) {
      writeBoth(s"line $i text;", asChars = true)
      writeBoth(s"line $i chars", asChars = false)
      sTest.newLine()
      sExpected.newLine()
    }
    sTest.close()
    sExpected.close()

    assertEquals(fileExpected.length(),fileTest.length())

    val expIn = new BufferedReader(new FileReader(fileExpected))
    val testIn = new BufferedReader(new FileReader(fileTest))

    var exp = expIn.readLine()
    while (exp ne null) {
      val actual = testIn.readLine()
      assertEquals(exp, actual)
      exp = expIn.readLine()
    }
    expIn.close()
    testIn.close()
    fileTest.delete()
    fileExpected.delete()
  }

  @Ignore
  @Test def showPerformance(): Unit = {
    //warmup
    for (i <- 1 to 1000) {
      writeIsSame()
    }

    val fileTest = File.createTempFile("FileUtilsTest", "t1").nn
    val fileExpected = File.createTempFile("FileUtilsTest", "t2").nn

    for (i <- 1 to 10) {
      val sTest = FileUtils.newAsyncBufferedWriter(fileTest.toPath.nn)
      val sExpected = new BufferedWriter(new FileWriter(fileExpected))

      val t1 = System.nanoTime()
      List.tabulate(10000) {i =>
        sTest.write(s"line $i text;")
        sTest.newLine()
      }
      val t2 = System.nanoTime()
      sTest.close()
      val t3 = System.nanoTime()
      List.tabulate(10000) {i =>
        sExpected.write(s"line $i text;")
        sExpected.newLine()
      }
      val t4 = System.nanoTime()
      sExpected.close()

      println(s"async    took ${t2 - t1} ns")
      println(s"buffered took ${t4 - t3} ns")

      fileTest.delete()
      fileExpected.delete()
    }
  }

}

