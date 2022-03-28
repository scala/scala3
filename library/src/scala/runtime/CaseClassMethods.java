package scala.runtime;

import java.util.Map;
import java.util.HashMap;
import java.lang.invoke.ConstantCallSite;
import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;
import static java.lang.invoke.MethodType.methodType;

/** Exposes bootstrap method for abstracting away boilerplate implementations
  * of case class methods.
  */
public final class CaseClassMethods {

  // Method handles for hashing
  private static final MethodHandle STRINGHASHCODE_HANDLE;
  private static final MethodHandle PRODUCTPREFIX_HANDLE;
  private static final MethodHandle MIX_HANDLE;
  private static final MethodHandle FINALIZEHASH_HANDLE;
  private static final MethodHandle ANYHASH_HANDLE;
  private static final Map<Class<?>, MethodHandle> PRIMITIVE_HASHER_HANDLES;
  private static final MethodHandle INITIALHASH_HANDLE;

  static {
    try {
      final MethodHandles.Lookup lookup = MethodHandles.lookup();
      final Class<?> STATICS_CLASS = Statics.class;

      STRINGHASHCODE_HANDLE = lookup.findVirtual(
        String.class,
        "hashCode",
        methodType(int.class)
      );
      PRODUCTPREFIX_HANDLE = lookup.findVirtual(
        scala.Product.class,
        "productPrefix",
        methodType(String.class)
      );
      MIX_HANDLE = lookup.findStatic(
        Statics.class,
        "mix",
        methodType(int.class, int.class, int.class)
      );
      FINALIZEHASH_HANDLE = lookup.findStatic(
        Statics.class,
        "finalizeHash",
        methodType(int.class, int.class, int.class)
      );
      ANYHASH_HANDLE = lookup.findStatic(
        Statics.class,
        "anyHash",
        methodType(int.class, Object.class)
      );
      INITIALHASH_HANDLE = MethodHandles.constant(int.class, 0xcafebabe);

      PRIMITIVE_HASHER_HANDLES = new HashMap<Class<?>, MethodHandle>();
      PRIMITIVE_HASHER_HANDLES.put(
        boolean.class,
        MethodHandles.guardWithTest(
          MethodHandles.identity(boolean.class),
          MethodHandles.dropArguments(MethodHandles.constant(int.class, 1231), 0, boolean.class),
          MethodHandles.dropArguments(MethodHandles.constant(int.class, 1237), 0, boolean.class)
        )
      );
      PRIMITIVE_HASHER_HANDLES.put(byte.class, MethodHandles.identity(int.class));
      PRIMITIVE_HASHER_HANDLES.put(short.class, MethodHandles.identity(int.class));
      PRIMITIVE_HASHER_HANDLES.put(char.class, MethodHandles.identity(int.class));
      PRIMITIVE_HASHER_HANDLES.put(int.class, MethodHandles.identity(int.class));
      PRIMITIVE_HASHER_HANDLES.put(
        long.class,
        lookup.findStatic(Statics.class, "longHash", methodType(int.class, long.class))
      );
      PRIMITIVE_HASHER_HANDLES.put(
        double.class,
        lookup.findStatic(Statics.class, "doubleHash", methodType(int.class, double.class))
      );
      PRIMITIVE_HASHER_HANDLES.put(
        float.class,
        lookup.findStatic(Statics.class, "floatHash", methodType(int.class, float.class))
      );

    } catch (ReflectiveOperationException e) {
      throw new RuntimeException(e);
    }
  }

  public static ConstantCallSite bootstrap(
    MethodHandles.Lookup lookup,
    String methodName,
    MethodType methodType,
    Class<?> caseClass,
    MethodHandle... fieldHandles
  ) throws Throwable {
    switch (methodName) {
      case "productElement":
      case "productElementName":
      case "equals":
        throw new UnsupportedOperationException();

      case "hashCode":
        var productPrefixHandle = lookup.findVirtual(
          caseClass,
          "productPrefix",
          methodType(String.class)
        );

        // Accumulator hasher of type `(LMyCaseClass;)I`
        var accHash = MethodHandles.filterReturnValue(
          MethodHandles.filterReturnValue(productPrefixHandle, STRINGHASHCODE_HANDLE),
          MethodHandles.insertArguments(MIX_HANDLE, 0, 0xcafebabe)
        );

        // Fold over fields, adding each field into the `accHash` handle
        for (final var fieldHandle : fieldHandles) {
          final Class<?> fieldType = fieldHandle.type().returnType();

          // Handle of type `(LMyCaseClass;)I` for hashing just this field
          final var fieldHash = MethodHandles.filterReturnValue(
            fieldHandle,
            PRIMITIVE_HASHER_HANDLES.getOrDefault(fieldType, ANYHASH_HANDLE.asType(methodType(int.class, fieldType)))
          );

          // Mix this field's hasher back into the accumulator hasher
          accHash = MethodHandles.permuteArguments(
            MethodHandles.filterArguments(MIX_HANDLE, 0, accHash, fieldHash),
            accHash.type(),
            0,
            0
          );
        }

        // Finalize the hash using the number of fields
        final var finalizedHash = MethodHandles.filterReturnValue(
          accHash,
          MethodHandles.insertArguments(FINALIZEHASH_HANDLE, 1, fieldHandles.length)
        );

        return new ConstantCallSite(finalizedHash);

      default:
        throw new IllegalArgumentException(methodName);
    }
  }

  public static int caseHashCode(scala.Product x) {
    return scala.util.hashing.MurmurHash3$.MODULE$.productHash(x);
  }
}
