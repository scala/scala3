
import scala.collection.mutable.ArrayBuffer

class PrivateTest[-M](private val v: ArrayBuffer[M]) // error

class PrivateTestMut[-M](private var v: ArrayBuffer[M]) // error

class PrivateTestParamOnly[-M](v: ArrayBuffer[M]) // no error
