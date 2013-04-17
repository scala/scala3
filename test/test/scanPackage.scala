package test

object scanPackage extends ScannerTest {

  def main(args: Array[String]): Unit =
    scanDir("src")

}