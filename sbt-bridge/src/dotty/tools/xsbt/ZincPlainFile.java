/*
 * Zinc - The incremental compiler for Scala.
 * Copyright Lightbend, Inc. and Mark Harrah
 */

package dotty.tools.xsbt;

import xsbti.PathBasedFile;

public class ZincPlainFile extends dotty.tools.io.PlainFile {
  public ZincPlainFile(PathBasedFile underlying) {
    super(new dotty.tools.io.Path(underlying.toPath()));
  }
}
