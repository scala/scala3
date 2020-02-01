class Impl

def (impl: Impl).prop(using Int) = ???//summon[Int]


def main(args: Array[String]): Unit = {
  given Int = 3
  println(new Impl().prop(using 3))
}