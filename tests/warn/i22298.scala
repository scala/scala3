package tests.warn.i22298

import java.lang.constant.MethodHandleDesc
import java.lang.constant.DirectMethodHandleDesc
import java.lang.constant.ClassDesc
import java.lang.constant.MethodTypeDesc

object Tester {
  val direct: DirectMethodHandleDesc =
    MethodHandleDesc.ofMethod(
      DirectMethodHandleDesc.Kind.STATIC,
      ClassDesc.of("java.lang.Integer"),
      "parseInt",
      MethodTypeDesc.ofDescriptor("someDescriptor")
    )

  val sealedJava: MethodHandleDesc =
      direct.asType(MethodTypeDesc.ofDescriptor("someAnotherDescriptor"))

  sealedJava match { // warn
    case _: DirectMethodHandleDesc => ()
  }
}