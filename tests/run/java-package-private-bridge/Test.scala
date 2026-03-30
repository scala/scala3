// scalajs: --skip

object Test {
  def main(args: Array[String]): Unit = {
    val hasBridge1 = classOf[other.ScalaChild].getDeclaredMethods.exists { m =>
      m.getName == "packagePrivate" &&
      m.getReturnType == classOf[Object]
    }
    assert(!hasBridge1,
      s"ScalaChild should not have bridge for packagePrivate")

    // JTable case
    val hasBridge2 = classOf[other.MyJTable].getDeclaredMethods.exists { m =>
      m.getName == "dropLocationForPoint" &&
      m.getReturnType == classOf[javax.swing.TransferHandler.DropLocation]
    }
    assert(!hasBridge2,
      s"MyJTable should not have bridge for dropLocationForPoint")
  }
}
