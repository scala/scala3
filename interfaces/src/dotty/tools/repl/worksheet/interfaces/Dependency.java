package dotty.tools.repl.worksheet.interfaces;

/** A resolved dependency that a worksheet declared for itself. */
public interface Dependency {
  /** @return The organization, e.g. `com.lihaoyi` */
  String organization();

  /** @return The module name, binary version suffix included, e.g. `os-lib_3` */
  String moduleName();

  /** @return The version, e.g. `0.11.8` */
  String version();
}
