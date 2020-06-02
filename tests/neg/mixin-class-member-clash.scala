trait T {
  class C
}
trait U {
  class C
}
class C extends T, U // error: error overriding class C in trait T
