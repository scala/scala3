/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package xsbt;

import xsbti.Logger;
import xsbti.Reporter;

public class ScaladocInterface {
  public void run(String[] args, Logger log, xsbti.Reporter delegate) {
    new DottydocRunner(args, log, delegate).run();
  }
}
