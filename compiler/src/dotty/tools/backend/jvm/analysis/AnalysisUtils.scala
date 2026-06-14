package dotty.tools.backend.jvm.analysis

import dotty.tools.backend.jvm.BTypes.InternalName
import dotty.tools.backend.jvm.{BCodeUtils, ClassBType}

import scala.tools.asm
import scala.tools.asm.{Handle, Opcodes}
import scala.tools.asm.tree.{AbstractInsnNode, FieldInsnNode, InvokeDynamicInsnNode, MethodInsnNode, MethodNode}

object AnalysisUtils {

  val primitiveTypes: Map[String, asm.Type] = Map(
    ("Unit", asm.Type.VOID_TYPE),
    ("Boolean", asm.Type.BOOLEAN_TYPE),
    ("Char", asm.Type.CHAR_TYPE),
    ("Byte", asm.Type.BYTE_TYPE),
    ("Short", asm.Type.SHORT_TYPE),
    ("Int", asm.Type.INT_TYPE),
    ("Float", asm.Type.FLOAT_TYPE),
    ("Long", asm.Type.LONG_TYPE),
    ("Double", asm.Type.DOUBLE_TYPE))

  def isModuleLoad(insn: AbstractInsnNode, nameMatches: InternalName => Boolean): Boolean = insn match {
    case fi: FieldInsnNode =>
      fi.getOpcode == Opcodes.GETSTATIC &&
        nameMatches(fi.owner) &&
        fi.name == "MODULE$" &&
        fi.desc.length == fi.owner.length + 2 &&
        fi.desc.regionMatches(1, fi.owner, 0, fi.owner.length)
    case _ => false
  }

  def isJavaLangStaticLoad(insn: AbstractInsnNode): Boolean = insn match {
    case fi: FieldInsnNode =>
      fi.getOpcode == Opcodes.GETSTATIC &&
        fi.owner.startsWith("java/lang/")
    case _ => false
  }

  // ==============================================================================================

  def isArrayGetLength(mi: MethodInsnNode): Boolean = mi.owner == "java/lang/reflect/Array" && mi.name == "getLength" && mi.desc == "(Ljava/lang/Object;)I"

  // If argument i of the method is null-checked, the bit `i+1` of the result is 1
  def argumentsNullCheckedByCallee(mi: MethodInsnNode): Long = {
    if (isArrayGetLength(mi)) 1
    else 0
  }

  // ==============================================================================================

  final case class LambdaMetaFactoryCall(indy: InvokeDynamicInsnNode, samMethodType: asm.Type, implMethod: Handle, instantiatedMethodType: asm.Type)

  object LambdaMetaFactoryCall {
    private val lambdaMetaFactoryMetafactoryHandle = new Handle(
      Opcodes.H_INVOKESTATIC,
      "java/lang/invoke/LambdaMetafactory",
      "metafactory",
      "(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodHandle;Ljava/lang/invoke/MethodType;)Ljava/lang/invoke/CallSite;",
      /* itf = */ false)

    private val lambdaMetaFactoryAltMetafactoryHandle = new Handle(
      Opcodes.H_INVOKESTATIC,
      "java/lang/invoke/LambdaMetafactory",
      "altMetafactory",
      "(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;Ljava/lang/invoke/MethodType;[Ljava/lang/Object;)Ljava/lang/invoke/CallSite;",
      /* itf = */ false)

    def unapply(insn: AbstractInsnNode): Option[(InvokeDynamicInsnNode, asm.Type, Handle, asm.Type, Array[asm.Type])] = insn match {
      case indy: InvokeDynamicInsnNode if indy.bsm == lambdaMetaFactoryMetafactoryHandle || indy.bsm == lambdaMetaFactoryAltMetafactoryHandle =>
        indy.bsmArgs match {
          case Array(samMethodType: asm.Type, implMethod: Handle, instantiatedMethodType: asm.Type, _*) =>
            // LambdaMetaFactory performs a number of automatic adaptations when invoking the lambda
            // implementation method (casting, boxing, unboxing, and primitive widening, see Javadoc).
            //
            // The closure optimizer supports only one of those adaptations: it will cast arguments
            // to the correct type when re-writing a closure call to the body method. Example:
            //
            //   val fun: String => String = l => l
            //   val l = List("")
            //   fun(l.head)
            //
            // The samMethodType of Function1 is `(Object)Object`, while the instantiatedMethodType
            // is `(String)String`. The return type of `List.head` is `Object`.
            //
            // The implMethod has the signature `C$anonfun(String)String`.
            //
            // At the closure callsite, we have an `INVOKEINTERFACE Function1.apply (Object)Object`,
            // so the object returned by `List.head` can be directly passed into the call (no cast).
            //
            // The closure object will cast the object to String before passing it to the implMethod.
            //
            // When re-writing the closure callsite to the implMethod, we have to insert a cast.
            //
            // The check below ensures that
            //   (1) the implMethod type has the expected arguments (captured types plus argument types
            //       from instantiatedMethodType)
            //   (2) the receiver of the implMethod matches the first captured type, if any, otherwise
            //       the first parameter type of instantiatedMethodType
            //   (3) all parameters that are not the same in samMethodType and instantiatedMethodType
            //       are reference types, so that we can insert casts to perform the same adaptation
            //       that the closure object would.

            val isStatic                   = implMethod.getTag == Opcodes.H_INVOKESTATIC
            val indyParamTypes             = asm.Type.getArgumentTypes(indy.desc)
            val instantiatedMethodArgTypes = instantiatedMethodType.getArgumentTypes

            val (receiverType, expectedImplMethodType) =
              if (isStatic) {
                val paramTypes = indyParamTypes ++ instantiatedMethodArgTypes
                (None, asm.Type.getMethodType(instantiatedMethodType.getReturnType, paramTypes*))
              } else if (implMethod.getTag == Opcodes.H_NEWINVOKESPECIAL) {
                (Some(instantiatedMethodType.getReturnType), asm.Type.getMethodType(asm.Type.VOID_TYPE, instantiatedMethodArgTypes*))
              } else {
                if (indyParamTypes.nonEmpty) {
                  val paramTypes = indyParamTypes.tail ++ instantiatedMethodArgTypes
                  (Some(indyParamTypes(0)), asm.Type.getMethodType(instantiatedMethodType.getReturnType, paramTypes*))
                } else {
                  val paramTypes = instantiatedMethodArgTypes.tail
                  (Some(instantiatedMethodArgTypes(0)), asm.Type.getMethodType(instantiatedMethodType.getReturnType, paramTypes*))
                }
              }

            val isIndyLambda =
              asm.Type.getType(implMethod.getDesc).getArgumentTypes.sameElements(expectedImplMethodType.getArgumentTypes) // (1)
                && receiverType.forall(rt => implMethod.getOwner == rt.getInternalName)  // (2)
                && samMethodType.getArgumentTypes.corresponds(instantiatedMethodArgTypes)((samArgType, instArgType) =>
                samArgType == instArgType || BCodeUtils.isReference(samArgType) && BCodeUtils.isReference(instArgType)) // (3)

            if (isIndyLambda) Some((indy, samMethodType, implMethod, instantiatedMethodType, indyParamTypes))
            else None

          case _ => None
        }
      case _ => None
    }
  }

  // ==============================================================================================

  def isRuntimeArrayLoadOrUpdate(insn: AbstractInsnNode): Boolean = insn.getOpcode == Opcodes.INVOKEVIRTUAL && {
    val mi = insn.asInstanceOf[MethodInsnNode]
    mi.owner == "scala/runtime/ScalaRunTime$" && {
      mi.name == "array_apply" && mi.desc == "(Ljava/lang/Object;I)Ljava/lang/Object;" ||
        mi.name == "array_update" && mi.desc == "(Ljava/lang/Object;ILjava/lang/Object;)V"
    }
  }

  private val primitiveManifestApplies: Map[String, String] = primitiveTypes map {
    case (k, _) => (k, s"()Lscala/reflect/ManifestFactory$$${k}Manifest;")
  }

  def isClassTagApply(mi: MethodInsnNode): Boolean = {
    mi.owner == "scala/reflect/ClassTag$" && {
      mi.name == "apply" && mi.desc == "(Ljava/lang/Class;)Lscala/reflect/ClassTag;" ||
        primitiveManifestApplies.get(mi.name).contains(mi.desc)
    }
  }

  // ==============================================================================================

  def isTraitSuperAccessor(method: MethodNode, owner: ClassBType): Boolean = {
    owner.isInterface &&
      BCodeUtils.isSyntheticMethod(method) &&
      method.name.endsWith("$") &&
      BCodeUtils.isStaticMethod(method) &&
      BCodeUtils.findSingleCall(method, mi => mi.itf && mi.getOpcode == Opcodes.INVOKESPECIAL && mi.name + "$" == method.name).nonEmpty
  }

  def isMixinForwarder(method: MethodNode, owner: ClassBType): Boolean = {
    !owner.isInterface &&
      // isSyntheticMethod(method) && // mixin forwarders are not synthetic it seems
      !BCodeUtils.isStaticMethod(method) &&
      BCodeUtils.findSingleCall(method, mi => mi.itf && mi.getOpcode == Opcodes.INVOKESTATIC && mi.name == method.name + "$").nonEmpty
  }

  def isTraitSuperAccessorOrMixinForwarder(method: MethodNode, owner: ClassBType): Boolean = {
    isTraitSuperAccessor(method, owner) || isMixinForwarder(method, owner)
  }

}
