/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package sys

import scala.language.`2.13`

/** A minimal Thread wrapper to enhance shutdown hooks.  It knows
 *  how to unregister itself.
 */
class ShutdownHookThread private (runnable: Runnable, name: String) extends Thread(runnable, name) {
  /** Removes this thread from the list of hooks to run at JVM shutdown.
   *
   *  @return `true` if this hook was still registered with the JVM and was
   *          successfully unregistered, `false` otherwise
   *  @throws IllegalStateException if the JVM is already shutting down, since
   *          hooks can no longer be removed once shutdown is in progress
   *  @throws SecurityException if a security manager is present and denies
   *          `RuntimePermission("shutdownHooks")`
   */
  def remove() = Runtime.getRuntime.removeShutdownHook(this)
}

object ShutdownHookThread {
  private var hookNameCount: Int = 0
  private def hookName(): String = synchronized {
    hookNameCount += 1
    "shutdownHook" + hookNameCount
  }
  /** Creates, names, and registers a shutdown hook to run the
   *  given code.
   *
   *  @param body the code to execute when the JVM shuts down
   *  @return the newly created and registered `ShutdownHookThread`
   */
  def apply(body: => Unit): ShutdownHookThread = {
    val t = new ShutdownHookThread(() => body, hookName())
    Runtime.getRuntime.addShutdownHook(t)
    t
  }
}
