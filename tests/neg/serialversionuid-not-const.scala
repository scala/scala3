@SerialVersionUID(13l.toLong) class C1 extends Serializable // error
@SerialVersionUID(13l) class C2 extends Serializable // OK
@SerialVersionUID(13.asInstanceOf[Long]) class C3 extends Serializable // error
@SerialVersionUID(Test.bippy) class C4 extends Serializable // error

object Test {
  val bippy = 13L
}
