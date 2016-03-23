class Arr {
  val s = new Array[Int](1)
  val s2 = new Array[Array[Int]](2)
  val s3: Array[Array[Int]] = Array.ofDim(2,3) 
  /*
public Arr();
    Code:
       0: aload_0
       1: invokespecial #14                 // Method java/lang/Object."<init>":()V
       4: aload_0
       5: iconst_1
       6: newarray       int
       8: checkcast     #15                 // class "[I"
      11: putfield      #17                 // Field s$$local:[I
      14: aload_0
      15: iconst_2
      16: anewarray     #15                 // class "[I"
      19: checkcast     #18                 // class "[[I"
      22: putfield      #20                 // Field s2$$local:[[I
      25: aload_0
      26: getstatic     #26                 // Field scala/Array$.MODULE$:Lscala/Array$;
      29: pop
      30: iconst_2
      31: iconst_3
      32: multianewarray #18,  2            // class "[[I"
      36: checkcast     #18                 // class "[[I"
      39: putfield      #28                 // Field s3$$local:[[I
      42: return
*/
}

object Test{
  def main(args: Array[String]): Unit = {
    new Arr
  }
}

