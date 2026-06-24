class Foo:
  def bar(x: Int) = {
    // @valhalla
    class Boo:// extends AnyVal: // Still runs but Error: Access Flags: Unmatched bit position 0x8 for location CLASS for class file format CURRENT_PREVIEW_FEATURES
    // added a static java flag to this class.
      def booAdd(x: Int, y: Int): Int = x + y + y

    println(new Boo().booAdd(x, 2))
  }
 
class Main:
    def main = {
      new Foo().bar(123)
    }