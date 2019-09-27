
// This test is like tests/pos/override-java-object-arg.scala, except that
// here we load the Java code from source, as opposed to a class file.
// In this case, the Java 'Object' type is turned into 'AnyRef', not 'Any'.

class S {

  def bar(): Unit = {
    val listener = new NotificationListener() {
      override def handleNotification(n: Notification|Null, emitter: Object|Null): Unit = {
      }
    }

    val listener2 = new NotificationListener() {
      override def handleNotification(n: Notification|Null, emitter: AnyRef|Null): Unit = {
      }
    }
  }

}
