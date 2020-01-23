class Impl

def (impl: Impl).prop with Int = ???//summon[Int]


def main(args: Array[String]): Unit = {
  given Int = 3
  println(new Impl().prop.with(3))
}