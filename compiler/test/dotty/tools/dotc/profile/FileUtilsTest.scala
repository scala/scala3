package dotty.tools.dotc.profile

import dotty.tools.io.FileExtension
import org.junit.Assert.*
import org.junit.*
import dotty.tools.nio.*

import scala.io.Codec

class FileUtilsTest {

  @Test def writeIsSame(): Unit = {
    val fileTest = File.createTemporaryOnDisk("FileUtilsTest", FileExtension.from("t1"))
    val fileExpected = File.createTemporaryOnDisk("FileUtilsTest", FileExtension.from("t2"))

    val sTest = FileUtils.newAsyncBufferedWriter(fileTest.writer(Codec.UTF8), threadsafe = false)
    val sExpected = fileExpected.writer(Codec.UTF8)

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

    assertEquals(fileExpected.size(), fileTest.size())

    val expIn = fileExpected.reader(Codec.UTF8)
    val testIn = fileTest.reader(Codec.UTF8)

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

    val fileTest = File.createTemporaryOnDisk("FileUtilsTest", FileExtension.from("t1"))
    val fileExpected = File.createTemporaryOnDisk("FileUtilsTest", FileExtension.from("t2"))

    for (i <- 1 to 10) {
      val sTest = FileUtils.newAsyncBufferedWriter(fileTest.writer(Codec.UTF8), threadsafe = false)
      val sExpected = fileExpected.writer(Codec.UTF8)

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

