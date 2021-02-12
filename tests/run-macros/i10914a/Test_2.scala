object Test {
  import Dsl.*

  inline def makeEnt = Entity("foo")

  // inline def entity = makeEnt // This case breaks to
  //inline def input = Input(entity)

  inline def input = Input(makeEnt)
  inline def contained = container(input)
  inline def output = pull(contained)

  def main(args: Array[String]): Unit = {
    println( output )
  }
}
