package xsbt;

import java.lang.reflect.Field;

import java.net.URL;
import java.net.URLClassLoader;

import java.util.WeakHashMap;

/**
 * A classloader to run the compiler
 * <p>
 * A CompilerClassLoader is constructed from a list of `urls` that need to be on
 * the classpath to run the compiler and the classloader used by sbt.
 * <p>
 * To understand why a custom classloader is needed for the compiler, let us
 * describe some alternatives that wouldn't work.
 * <ul>
 * <li>`new URLClassLoader(urls)`:
 *   The compiler contains sbt phases that callback to sbt using the `xsbti.*`
 *   interfaces. If `urls` does not contain the sbt interfaces we'll get a
 *   `ClassNotFoundException` in the compiler when we try to use them, if
 *   `urls` does contain the interfaces we'll get a `ClassCastException` or a
 *   `LinkageError` because if the same class is loaded by two different
 *   classloaders, they are considered distinct by the JVM.
 * <li>`new URLClassLoader(urls, sbtLoader)`:
 *    Because of the JVM delegation model, this means that we will only load
 *    a class from `urls` if it's not present in the parent `sbtLoader`, but
 *    sbt uses its own version of the scala compiler and scala library which
 *    is not the one we need to run the compiler.
 * </ul>
 * <p>
 * Our solution is to implement a subclass of URLClassLoader with no parent, instead
 * we override `loadClass` to load the `xsbti.*` interfaces from `sbtLoader`.
 */
public class CompilerClassLoader extends URLClassLoader {
  private final ClassLoader sbtLoader;

  public CompilerClassLoader(URL[] urls, ClassLoader sbtLoader) {
    super(urls, null);
    this.sbtLoader = sbtLoader;
  }

  @Override
  public Class<?> loadClass(String className, boolean resolve) throws ClassNotFoundException {
    if (className.startsWith("xsbti.")) {
      // We can't use the loadClass overload with two arguments because it's
      // protected, but we can do the same by hand (the classloader instance
      // from which we call resolveClass does not matter).
      Class<?> c = sbtLoader.loadClass(className);
      if (resolve)
        resolveClass(c);
      return c;
    } else {
      return super.loadClass(className, resolve);
    }
  }

  /**
   * Cache the result of `fixBridgeLoader`.
   * <p>
   * Reusing ClassLoaders is important for warm performance since otherwise the
   * JIT code cache for the compiler will be discarded between every call to
   * the sbt `compile` task.
   */
  private static WeakHashMap<ClassLoader, ClassLoader> fixedLoaderCache = new WeakHashMap<>();

  /**
   * Fix the compiler bridge ClassLoader
   * <p>
   * Soundtrack: https://www.youtube.com/watch?v=imamcajBEJs
   * <p>
   * The classloader that we get from sbt looks like:
   * <p>
   *   URLClassLoader(bridgeURLs,
   *     DualLoader(scalaLoader, notXsbtiFilter, sbtLoader, xsbtiFilter))
   * <p>
   * DualLoader will load the `xsbti.*` interfaces using `sbtLoader` and
   * everything else with `scalaLoader`. Once we have loaded the dotty Main
   * class using `scalaLoader`, subsequent classes in the dotty compiler will
   * also be loaded by `scalaLoader` and _not_ by the DualLoader. But the sbt
   * compiler phases are part of dotty and still need access to the `xsbti.*`
   * interfaces in `sbtLoader`, therefore DualLoader does not work for us
   * (this issue is not present with scalac because the sbt phases are
   * currently defined in the compiler bridge itself, not in scalac).
   * <p>
   * CompilerClassLoader is a replacement for DualLoader. Until we can fix
   * this in sbt proper, we need to use reflection to construct our own
   * fixed classloader:
   * <p>
   *   URLClassLoader(bridgeURLs,
   *     CompilerClassLoader(scalaLoader.getURLs, sbtLoader))
   *
   *  @param bridgeLoader  The classloader that sbt uses to load the compiler bridge
   *  @return A fixed classloader that works with dotty
   */
  synchronized public static ClassLoader fixBridgeLoader(ClassLoader bridgeLoader) {
    return fixedLoaderCache.computeIfAbsent(bridgeLoader, k -> computeFixedLoader(k));
  }

  private static ClassLoader computeFixedLoader(ClassLoader bridgeLoader) {
    URLClassLoader urlBridgeLoader = (URLClassLoader) bridgeLoader;
    ClassLoader dualLoader = urlBridgeLoader.getParent();
    Class<?> dualLoaderClass = dualLoader.getClass();

    try {
      // DualLoader.parentA and DualLoader.parentB are private
      Field parentAField = dualLoaderClass.getDeclaredField("parentA");
      parentAField.setAccessible(true);
      Field parentBField = dualLoaderClass.getDeclaredField("parentB");
      parentBField.setAccessible(true);
      URLClassLoader scalaLoader = (URLClassLoader) parentAField.get(dualLoader);
      ClassLoader sbtLoader = (ClassLoader) parentBField.get(dualLoader);

      URL[] bridgeURLs = urlBridgeLoader.getURLs();
      return new URLClassLoader(bridgeURLs,
        new CompilerClassLoader(scalaLoader.getURLs(), sbtLoader));
    } catch (NoSuchFieldException | IllegalAccessException e) {
      throw new RuntimeException(e);
    }
  }
}