case class HasSingleField(f: HasSingleField)

object Test {

  def main(args: Array[String]) = {
    val s: Object = HasSingleField(null)
    s match {
      case Matcher(self) => 
        assert(self ne null)
    }
  }
}

object Matcher {
 def unapply(x: Object): Option[HasSingleField] = {
   if (x.isInstanceOf[HasSingleField]) 
     Some(x.asInstanceOf[HasSingleField]) 
   else 
     None 
  }
}
