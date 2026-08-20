object T:
  val x = toString"huh" // warn
  val y = hashCode"hoh" // warn
  val z = getClass"hah" // warn

  { val x = "X"; eq" $x" } // warn
  { val x = "X"; ne" $x" } // warn

  val ok = StringContext("huh").toString
