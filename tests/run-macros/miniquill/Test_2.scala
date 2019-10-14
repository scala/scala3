import Miniquill._

@main
def Test = {
  val pi /*: Quoted[Double] */ = quote(3.14159)

  // run(pi)
  case class Circle(radius: Float)

  val areas = quote {
    query[Circle].map[Double]((c: Circle) => pi * c.radius): EntityQuery[Double]
  }
  /*
    querySchema("Circle"): Playground.this.ctx.Quoted[Playground.this.ctx.EntityQuery[Playground.this.Circle]]{
      def quoted: io.getquill.ast.Entity;
      def ast: io.getquill.ast.Entity;
      def id2124864049(): Unit;
      val liftings: Object
  */

  run(areas)
  // SELECT (3.14159 * c.radius) * c.radius FROM circle c
}