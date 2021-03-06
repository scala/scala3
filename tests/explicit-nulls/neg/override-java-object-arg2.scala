
import javax.management.{Notification, NotificationEmitter, NotificationListener}

class Foo {

  def bar(): Unit = {
    val listener4 = new NotificationListener() {
      def handleNotification(n: Notification|Null, emitter: Object): Unit = { // error
      }
    }
  }

}
