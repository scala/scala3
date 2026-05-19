object Test {
  type T = String

/*  val a /* : Class[T]                  */ = classOf[T]                        // [Ljava/lang/String;
  println(a)
*/
  val b /* : ClassTag[T]               */ = reflect.classTag[T]               // ClassTag(classOf[java.lang.String])
/*  println(b)

  val d /* : ClassTag[T with U]        */ = reflect.classTag[T with U]        // ClassTag(classOf[java.lang.String])
  println(d)
  val e /* : Class[Array[T with U]]    */ = classOf[Array[T with U]]          // [Ljava/lang/String;
  println(e)
  val f /* : ClassTag[Array[T with U]] */ = reflect.classTag[Array[T with U]] // ClassTag(arrayClass(classOf[java.lang.String]))
  println(f)
  val g /* : Class[Meter]              */ = classOf[Meter]                    // [LMeter;
  println(g)
  val h /* : ClassTag[Meter]           */ = reflect.classTag[Meter]           // ClassTag(classOf[Meter])
  println(h)
*/
  def main(args: Array[String]): Unit = ()
}

//class Meter(val i: Int) extends AnyVal
