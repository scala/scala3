package dotty.tools.debug

import com.sun.jdi.*
import dotty.tools.io.*
import dotty.tools.vulpix.TestFlags

import scala.jdk.CollectionConverters.*

class ExpressionEvaluator(
  sources: Map[String, JPath],
  options: Array[String],
  classPath: String,
  outputDir: JPath
):
  private val compiler = ExpressionCompilerBridge()
  private var uniqueID: Int = 1

  private class EvaluationException(message: String, cause: InvocationException)
    extends Exception(message, cause)

  /** returns the value of the evaluated expression or compiler errors */
  def evaluate(expression: String, thread: ThreadReference): Either[String, String] =
    // We evaluate the expression at the top frame of the stack
    val frame = thread.frame(0)

    // Extract everything from the frame now because, as soon as we start using the thread
    // for remote execution, the frame becomes invalid
    val localVariables = frame.visibleVariables.asScala.toSeq
    val values = localVariables.map(frame.getValue)
    val location = frame.location
    val thisRef = frame.thisObject // null in a static context

    for expressionClassName <- compile(expression, location, localVariables) yield
      // we don't need to create a new classloader because we compiled the expression class
      // in the same outputDir as main classes
      val classLoader = location.declaringType.classLoader
      val expressionClass = thread.loadClass(classLoader, expressionClassName)

      val nameArray = thread.createArray(
        "java.lang.String",
        localVariables.map(v => thread.virtualMachine.mirrorOf(v.name))
      )
      val valueArray = thread.createArray("java.lang.Object", values.map(thread.boxIfPrimitive))
      val args = Seq(thisRef, nameArray, valueArray)

      val exprRef = thread.newInstance(expressionClass, args)
      try
        val output =
          thread.invoke[ObjectReference](exprRef, "evaluate", "()Ljava/lang/Object;", Seq.empty)
        updateVariables(thread, valueArray)
        thread.invoke[StringReference](output, "toString", "()Ljava/lang/String;", Seq.empty).value
      catch case e: EvaluationException =>
        // if expr.evaluate() throws an exception, we return exception.toString as the value
        // to distinguish it from an evaluation error
        // throwing an exception is a valid result of evaluation
        e.getMessage
  end evaluate

  /** compiles the expression and returns the new expression class name to load, or compiler errors */
  private def compile(
    expression: String,
    location: Location,
    localVariables: Seq[LocalVariable]
  ): Either[String, String] =
    // We assume there is no 2 files with the same name
    val sourceFile = sources(location.sourceName)
    val packageName = getPackageName(location.declaringType)
    val outputClassName = getUniqueClassName()
    val errorBuilder = StringBuilder()
    val config = ExpressionCompilerConfig(
      packageName = packageName,
      outputClassName = outputClassName,
      breakpointLine = location.lineNumber,
      expression = expression,
      localVariables = localVariables.toSet.map(_.name).asJava,
      errorReporter = errorMsg => errorBuilder.append(errorMsg),
      testMode = true
    )
    val success = compiler.run(outputDir, classPath, options, sourceFile, config)
    val fullyQualifiedClassName =
      if packageName.isEmpty then outputClassName else s"$packageName.$outputClassName"
    if success then Right(fullyQualifiedClassName) else Left(errorBuilder.toString)
  end compile

  private def updateVariables(thread: ThreadReference, valueArray: ArrayReference): Unit =
    // the frame reference change after each remote execution
    def frame = thread.frame(0)
    frame
      .visibleVariables
      .asScala
      .toSeq
      .zip(valueArray.getValues.asScala)
      .map: (variable, value) =>
        val preparedValue =
          if variable.`type`.isInstanceOf[PrimitiveType] then thread.unboxIfPrimitive(value)
          else value
        frame.setValue(variable, preparedValue)

  private def getPackageName(tpe: ReferenceType): String =
    tpe.name.split('.').dropRight(1).mkString(".")

  private def getUniqueClassName(): String =
    val id = uniqueID
    uniqueID += 1
    "Expression" + id

  extension (thread: ThreadReference)
    private def boxIfPrimitive(value: Value): ObjectReference =
      value match
        case value: PrimitiveValue => box(value)
        case ref: ObjectReference => ref

    private def unboxIfPrimitive(value: Value): Value =
      import ExpressionEvaluator.unboxMethods
      value match
        case ref: ObjectReference if unboxMethods.contains(ref.referenceType.name) =>
          val (methodName, sig) = unboxMethods(ref.referenceType.name)
          invoke(ref, methodName, sig, Seq.empty)
        case _ => value

    private def box(value: PrimitiveValue): ObjectReference =
      val (className, sig) = value match
        case _: BooleanValue => ("java.lang.Boolean", "(Ljava/lang/String;)Ljava/lang/Boolean;")
        case _: ByteValue => ("java.lang.Byte", "(Ljava/lang/String;)Ljava/lang/Byte;")
        case _: CharValue => ("java.lang.Character", "(C)Ljava/lang/Character;")
        case _: DoubleValue => ("java.lang.Double", "(Ljava/lang/String;)Ljava/lang/Double;")
        case _: FloatValue => ("java.lang.Float", "(Ljava/lang/String;)Ljava/lang/Float;")
        case _: IntegerValue => ("java.lang.Integer", "(Ljava/lang/String;)Ljava/lang/Integer;")
        case _: LongValue => ("java.lang.Long", "(Ljava/lang/String;)Ljava/lang/Long;")
        case _: ShortValue => ("java.lang.Short", "(Ljava/lang/String;)Ljava/lang/Short;")
      val cls = getClass(className)
      val args = value match
        case c: CharValue => Seq(c)
        case value => Seq(mirrorOf(value.toString))
      invokeStatic(cls, "valueOf", sig, args)

    private def createArray(arrayType: String, values: Seq[Value]): ArrayReference =
      val arrayClassObject = getClass(arrayType).classObject
      val reflectArrayClass = getClass("java.lang.reflect.Array")
      val args = Seq(arrayClassObject, mirrorOf(values.size))
      val sig = "(Ljava/lang/Class;I)Ljava/lang/Object;"
      val arrayRef = invokeStatic[ArrayReference](reflectArrayClass, "newInstance", sig, args)
      arrayRef.setValues(values.asJava)
      arrayRef

    /** Get the remote class if it is already loaded. Otherwise you should use loadClass. */
    private def getClass(className: String): ClassType =
      thread.virtualMachine.classesByName(className).get(0).asInstanceOf[ClassType]

    private def loadClass(classLoader: ClassLoaderReference, className: String): ClassType =
      // Calling classLoader.loadClass would create useless class object which throws
      // ClassNotPreparedException. We use java.lang.Class.forName instead.
      val classClass = getClass("java.lang.Class")
      val args = Seq(mirrorOf(className), mirrorOf(true), classLoader)
      val sig = "(Ljava/lang/String;ZLjava/lang/ClassLoader;)Ljava/lang/Class;"
      invokeStatic[ClassObjectReference](classClass, "forName", sig, args)
        .reflectedType
        .asInstanceOf[ClassType]

    private def invokeStatic[T <: Value](
      cls: ClassType,
      methodName: String,
      sig: String,
      args: Seq[Value]
    ): T =
      val method = cls.methodsByName(methodName, sig).get(0)
      remotely:
        cls.invokeMethod(thread, method, args.asJava, ObjectReference.INVOKE_SINGLE_THREADED)

    // we assume there is a single constructor, otherwise we need to add sig as parameter
    private def newInstance(cls: ClassType, args: Seq[Value]): ObjectReference =
      val constructor = cls.methodsByName("<init>").get(0)
      remotely:
        cls.newInstance(thread, constructor, args.asJava, ObjectReference.INVOKE_SINGLE_THREADED)

    private def invoke[T <: Value](
      ref: ObjectReference,
      methodName: String,
      sig: String,
      args: Seq[Value]
    ): T =
      val method = ref.referenceType.methodsByName(methodName, sig).get(0)
      remotely:
        ref.invokeMethod(thread, method, args.asJava, ObjectReference.INVOKE_SINGLE_THREADED)

    /** wrapper for safe remote execution:
      * - it catches InvocationException to extract the message of the remote exception
      * - it disables GC on the returned value
      */
    private def remotely[T <: Value](value: => Value): T =
      val res =
        try value
        catch case invocationException: InvocationException =>
          val sig = "()Ljava/lang/String;"
          val message =
            invoke[StringReference](invocationException.exception, "toString", sig, List())
          throw new EvaluationException(message.value, invocationException)
      // Prevent object created by the debugger to be garbage collected
      // In theory we should re-enable collection later to avoid memory leak
      // But maybe it is okay to have a few leaks in the tested debuggee
      res match
        case ref: ObjectReference => ref.disableCollection()
        case _ =>
      res.asInstanceOf[T]

    private def mirrorOf(value: String): StringReference = thread.virtualMachine.mirrorOf(value)
    private def mirrorOf(value: Int): IntegerValue = thread.virtualMachine.mirrorOf(value)
    private def mirrorOf(value: Boolean): BooleanValue = thread.virtualMachine.mirrorOf(value)
  end extension
end ExpressionEvaluator

object ExpressionEvaluator:
  private val unboxMethods = Map(
    "java.lang.Boolean" -> ("booleanValue", "()Z"),
    "java.lang.Byte" -> ("byteValue", "()B"),
    "java.lang.Character" -> ("charValue", "()C"),
    "java.lang.Double" -> ("doubleValue", "()D"),
    "java.lang.Float" -> ("floatValue", "()F"),
    "java.lang.Integer" -> ("intValue", "()I"),
    "java.lang.Long" -> ("longValue", "()J"),
    "java.lang.Short" -> ("shortValue", "(S)")
  )


  def apply(
    sources: Array[JFile],
    flags: TestFlags,
    classPath: String,
    outputDir: JFile
  ): ExpressionEvaluator =
    val sourceMap = sources.map(s => s.getName -> s.toPath).toMap
    val filteredOptions = flags.options.filterNot(_ == "-Ycheck:all")
    new ExpressionEvaluator(sourceMap, filteredOptions, classPath, outputDir.toPath)
