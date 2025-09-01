package demo

import scala.scalajs.js

def bar: js.Promise[Int] = js.Promise.resolve(()).`then`(_ => 1)
