package dotty.tools.dotc
package tastyreflect

trait CoreImpl extends scala.tasty.reflect.Core {

  final val kernel: KernelImpl = new KernelImpl

}
