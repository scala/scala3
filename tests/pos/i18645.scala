trait Printable {
  def pprint(v: () => String): Unit = {
    println(v())
  }
}

extension (ctx: Printable)
  def pprint(f: () => Int): Unit = {
    ctx.pprint(() => f().toString)
  }

val x = new Printable {}

def test =
  x.pprint(() => ( 234 ))
  x.pprint(() => { 123 }) 