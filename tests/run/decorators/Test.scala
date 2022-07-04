// scalajs: --skip

object Test:
  def main(args: Array[String]) =
    def testAdd(args: String) =
      println(s"> java add $args")
      add.main(args.split(" "))
      println()
    def testAddAll(args: String) =
      println(s"> java addAll $args")
      addAll.main(args.split(" "))
      println()

    testAdd("2 3")
    testAdd("4")
    testAdd("--num 10 --inc -2")
    testAdd("--num 10")
    testAdd("--help")
    testAdd("")
    testAdd("1 2 3 4")
    testAdd("-n 1 -i 2")
    testAdd("true 10")
    testAdd("true false")
    testAdd("true false 10")
    testAdd("--inc 10 --num 20")
    testAdd("binary 10 01")
    testAddAll("1 2 3 4 5")
    testAddAll("--nums")
    testAddAll("--nums 33 44")
    testAddAll("true 1 2 3")
    testAddAll("--help")
end Test
