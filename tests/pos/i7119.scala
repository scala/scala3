class Impl

def (impl: Impl) prop given Int = ???//summon[Int]


def main(args: Array[String]): Unit = {
  given Int = 3
  println(new Impl().prop given 3)
}