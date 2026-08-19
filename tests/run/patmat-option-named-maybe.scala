//> using options -Yexplicit-nulls
import language.experimental.magic
case class HasSingleField(f: HasSingleField)

object Test {

  def main(args: Array[String]) = {
    val s: Object = HasSingleField(null.asInstanceOf[HasSingleField])
    s match {
      case Matcher(self) => 
        assert(self ne null)
    }
  }
}

object Matcher {
 def unapply(x: Object): HasSingleField? = {
   if (x.isInstanceOf[HasSingleField]) 
     x.asInstanceOf[HasSingleField] 
   else 
     null 
  }
}
