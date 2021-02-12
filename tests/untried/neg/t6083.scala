object conv {
  implicit def i2s(i: Int): String = ""
}
import conv.*

class annot(value: String) extends annotation.ClassfileAnnotation
@annot(101) class C
