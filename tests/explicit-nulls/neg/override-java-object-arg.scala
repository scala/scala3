
// Test that we can properly override Java methods where an argument has type 'Object'.
// See pos/override-java-object-arg.scala for context.

import javax.management.{Notification, NotificationEmitter, NotificationListener}

class Foo {

  def bar(): Unit = {
    val listener = new NotificationListener() {
      override def handleNotification(n: Notification|Null, emitter: Object): Unit = { // error: method handleNotification overrides nothing
      }
    }

    val listener2 = new NotificationListener() {
      override def handleNotification(n: Notification|Null, emitter: Object|Null): Unit = { // ok
      }
    }

    val listener3 = new NotificationListener() {
      override def handleNotification(n: Notification, emitter: Object|Null): Unit = { // error: method handleNotification overrides nothing
      }
    }
  }
}

