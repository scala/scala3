/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package xsbt;

import xsbti.Problem;

public class InterfaceCompileFailed extends xsbti.CompileFailed {
  private final String[] _arguments;
  private final Problem[] _problems;

  public InterfaceCompileFailed(String[] arguments, Problem[] problems) {
    super();
    this._arguments = arguments;
    this._problems = problems;
  }

  public String[] arguments() {
    return _arguments;
  }

  public Problem[] problems() {
    return _problems;
  }

  @Override
  public String toString() {
    return "Compilation failed";
  }
}
