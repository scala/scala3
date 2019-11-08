
import javax.management.{Notification, NotificationEmitter, NotificationListener}

class Foo {

  def bar(): Unit = {
    val listener4 = new NotificationListener() { // error: duplicate symbol error
      def handleNotification(n: Notification|Null, emitter: Object): Unit = {
      }
    }
  }

}
