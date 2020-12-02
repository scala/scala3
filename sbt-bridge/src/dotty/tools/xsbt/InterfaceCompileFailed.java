/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package dotty.tools.xsbt;

import xsbti.Problem;

public class InterfaceCompileFailed extends xsbti.CompileFailed {
  private final String[] _arguments;
  private final Problem[] _problems;
  private final String _toString;

  public InterfaceCompileFailed(String[] arguments, Problem[] problems, String toString) {
    super();
    this._arguments = arguments;
    this._problems = problems;
    this._toString = toString;
  }

  public String[] arguments() {
    return _arguments;
  }

  public Problem[] problems() {
    return _problems;
  }

  @Override
  public String toString() {
    return _toString;
  }
}
