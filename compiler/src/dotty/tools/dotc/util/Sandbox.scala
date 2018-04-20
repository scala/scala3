package dotty.tools.dotc.util


import java.lang.reflect.{InvocationTargetException, ReflectPermission}
import java.security.Permission

import scala.quoted.QuoteError

object Sandbox {

  /** Timeout in milliseconds */
  final val timeout = 3000 // TODO add a flag to allow custom timeouts

  def runInSecuredThread[T](thunk: => T): T = {
    runWithSandboxSecurityManager { securityManager =>
      class SandboxThread extends Thread {
        var result: scala.util.Try[T] =
          scala.util.Failure(new Exception("Sandbox failed with a fatal error"))
        override def run(): Unit = {
          result = scala.util.Try {
            securityManager.enable() // Enable security manager on this thread
            thunk
          }
        }
      }
      val thread = new SandboxThread
      thread.start()
      thread.join(timeout)
      if (thread.isAlive) {
        // TODO kill the thread?
        throw new InvocationTargetException(new QuoteError(s"Failed to evaluate inlined quote. Caused by timeout ($timeout ms)."))
      } else thread.result.fold[T](throw _, identity)
    }
  }

  private def runWithSandboxSecurityManager[T](run: SandboxSecurityManager => T): T = {
    val ssm: SandboxSecurityManager = synchronized {
      System.getSecurityManager match {
        case ssm: SandboxSecurityManager =>
          assert(ssm.running > 0)
          ssm.running += 1
          ssm
        case sm =>
          assert(sm == null)
          val ssm = new SandboxSecurityManager
          System.setSecurityManager(ssm)
          ssm
      }
    }
    try run(ssm)
    finally synchronized {
      ssm.running -= 1
      assert(ssm.running >= 0)
      if (ssm.running == 0) {
        assert(System.getSecurityManager eq ssm)
        System.setSecurityManager(null)
      }
    }
  }

  /** A security manager that can be enabled on individual threads.
   *
   *  Inspired by https://github.com/alphaloop/selective-security-manager
   */
  private class SandboxSecurityManager extends SecurityManager {

    @volatile private[Sandbox] var running: Int = 1

    private[this] val enabledFlag: ThreadLocal[Boolean] = new ThreadLocal[Boolean]() {
      override protected def initialValue(): Boolean = false
    }

    def enable(): Unit = {
      enabledFlag.set(true)
    }

    override def checkPermission(permission: Permission): Unit = {
      if (enabledFlag.get() && !isClassLoading)
        super.checkPermission(permission)
    }

    override def checkPermission(permission: Permission, context: Object): Unit = {
      if (enabledFlag.get())
        super.checkPermission(permission, context)
    }

    private def isClassLoading: Boolean = {
      try {
        enabledFlag.set(false) // Disable security to do the check
        Thread.currentThread().getStackTrace.exists(elem => elem.getClassName == "java.lang.ClassLoader")
      } finally {
        enabledFlag.set(true)
      }
    }
  }
}
