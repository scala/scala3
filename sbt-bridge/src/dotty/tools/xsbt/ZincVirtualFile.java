/*
 * Zinc - The incremental compiler for Scala.
 * Copyright Lightbend, Inc. and Mark Harrah
 */

package dotty.tools.xsbt;

import dotty.tools.io.Streamable;
import xsbti.VirtualFile;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

public class ZincVirtualFile extends dotty.tools.io.VirtualFile {

  public ZincVirtualFile(VirtualFile underlying) throws IOException {
    super(underlying.name(), underlying.id());

    // fill in the content
    try (OutputStream output = output()) {
      try (InputStream input = underlying.input()) {
        Streamable.Bytes bytes = new Streamable.Bytes() {
          @Override
          public InputStream inputStream() {
            return input;
          }
        };
        output.write(bytes.toByteArray());
      }
    }
  }
}
