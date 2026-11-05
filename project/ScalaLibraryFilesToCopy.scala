package dotty.tools.sbtplugin

object ScalaLibraryFilesToCopy {
  lazy val filesToCopy = Set(
    "scala/Function0",
    "scala/Function1",
    "scala/Function2",
    "scala/Product1",
    "scala/Product2",
    "scala/Tuple1",
    "scala/Tuple2",
    "scala/collection/ArrayOps",
    "scala/collection/Stepper",
    "scala/collection/DoubleStepper",
    "scala/collection/IntStepper",
    "scala/collection/LongStepper",
    "scala/collection/immutable/DoubleVectorStepper",
    "scala/collection/immutable/IntVectorStepper",
    "scala/collection/immutable/LongVectorStepper",
    "scala/collection/immutable/Range",
    "scala/jdk/Accumulator",
    "scala/jdk/DoubleAccumulator",
    "scala/jdk/IntAccumulator",
    "scala/jdk/LongAccumulator",
    "scala/jdk/FunctionWrappers$FromJavaDoubleBinaryOperator",
    "scala/jdk/FunctionWrappers$FromJavaBooleanSupplier",
    "scala/jdk/FunctionWrappers$FromJavaDoubleConsumer",
    "scala/jdk/FunctionWrappers$FromJavaDoublePredicate",
    "scala/jdk/FunctionWrappers$FromJavaDoubleSupplier",
    "scala/jdk/FunctionWrappers$FromJavaDoubleToIntFunction",
    "scala/jdk/FunctionWrappers$FromJavaDoubleToLongFunction",
    "scala/jdk/FunctionWrappers$FromJavaIntBinaryOperator",
    "scala/jdk/FunctionWrappers$FromJavaDoubleUnaryOperator",
    "scala/jdk/FunctionWrappers$FromJavaIntPredicate",
    "scala/jdk/FunctionWrappers$FromJavaIntConsumer",
    "scala/jdk/FunctionWrappers$FromJavaIntSupplier",
    "scala/jdk/FunctionWrappers$FromJavaIntToDoubleFunction",
    "scala/jdk/FunctionWrappers$FromJavaIntToLongFunction",
    "scala/jdk/FunctionWrappers$FromJavaIntUnaryOperator",
    "scala/jdk/FunctionWrappers$FromJavaLongBinaryOperator",
    "scala/jdk/FunctionWrappers$FromJavaLongConsumer",
    "scala/jdk/FunctionWrappers$FromJavaLongPredicate",
    "scala/jdk/FunctionWrappers$FromJavaLongSupplier",
    "scala/jdk/FunctionWrappers$FromJavaLongToDoubleFunction",
    "scala/jdk/FunctionWrappers$FromJavaLongToIntFunction",
    "scala/jdk/FunctionWrappers$FromJavaLongUnaryOperator",
    "scala/runtime/AbstractFunction0",
    "scala/runtime/AbstractFunction1",
    "scala/runtime/AbstractFunction2",
    "scala/runtime/AbstractPartialFunction",
    "scala/runtime/NonLocalReturnControl",
    "scala/util/hashing/MurmurHash3",
  )

  /** Files that match `filesToCopy` but must keep the version compiled by Scala 3.
   *  These are the companion objects of the specialized `Function0`, `Function1` and `Function2`.
   *  They are not specialized, and they define Scala 3 extension methods (`applyUsing`)
   *  that the Scala 2.13 versions do not have.
   *  (Scala 2.13 has no `Function0` and `Function2` companions now, but this keeps them safe if it adds them.)
   */
  lazy val filesToKeep = Set(
    "scala/Function0$",
    "scala/Function1$",
    "scala/Function2$",
  )
}
