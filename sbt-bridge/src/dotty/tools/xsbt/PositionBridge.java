/*
 * Zinc - The incremental compiler for Scala.
 * Copyright Lightbend, Inc. and Mark Harrah
 */

package dotty.tools.xsbt;

import dotty.tools.dotc.util.SourceFile;
import dotty.tools.dotc.util.SourcePosition;
import dotty.tools.io.AbstractFile;
import xsbti.Position;

import java.io.File;
import java.util.Optional;

public class PositionBridge implements Position {
  private final SourcePosition pos;
  private final SourceFile src;

  public static final Position noPosition = new Position() {
    public Optional<java.io.File> sourceFile() {
      return Optional.empty();
    }
    public Optional<String> sourcePath() {
      return Optional.empty();
    }
    public Optional<Integer> line() {
      return Optional.empty();
    }
    public String lineContent() {
      return "";
    }
    public Optional<Integer> offset() {
      return Optional.empty();
    }
    public Optional<Integer> pointer() {
      return Optional.empty();
    }
    public Optional<String> pointerSpace() {
      return Optional.empty();
    }

    public String toString() {
      return "";
    }
  };

  public PositionBridge(SourcePosition pos, SourceFile src) {
    this.pos = pos;
    this.src = src;
  }

  @Override
  public Optional<Integer> line() {
    if (src.content().length == 0)
      return Optional.empty();

    int line = pos.line();
    if (line == -1) return Optional.empty();
    else return Optional.of(line + 1);
  }

  @Override
  public String lineContent() {
    if (src.content().length == 0)
      return "";

    String lineContent = pos.lineContent();
    if (lineContent.endsWith("\r\n")) {
      return lineContent.substring(0, lineContent.length() - 2);
    } else if (lineContent.endsWith("\n") || lineContent.endsWith("\u000c")) {
      return lineContent.substring(0, lineContent.length() - 1);
    } else {
      return lineContent;
    }
  }

  @Override
  public Optional<Integer> offset() {
    return Optional.of(pos.point());
  }

  @Override
  public Optional<String> sourcePath() {
    if (!src.exists())
      return Optional.empty();

    AbstractFile sourceFile = pos.source().file();
    if (sourceFile instanceof ZincPlainFile) {
      return Optional.of(((ZincPlainFile) sourceFile).underlying().id());
    } else if (sourceFile instanceof ZincVirtualFile) {
      return Optional.of(((ZincVirtualFile) sourceFile).underlying().id());
    } else {
      return Optional.of(sourceFile.path());
    }
  }

  @Override
  public Optional<File> sourceFile() {
    if (!src.exists()) return Optional.empty();
    return Optional.ofNullable(src.file().file());
  }

  @Override
  public Optional<Integer> pointer() {
    if (src.content().length == 0) return Optional.empty();
    return Optional.of(pos.point() - src.startOfLine(pos.point()));
  }

  @Override
  public Optional<String> pointerSpace() {
    if (!pointer().isPresent())
      return Optional.empty();

    // Don't crash if pointer is out-of-bounds (happens with some macros)
    int fixedPointer = Math.min(pointer().get(), lineContent().length());
    String lineContent = this.lineContent();
    StringBuilder result = new StringBuilder();
    for (int i = 0; i < fixedPointer; i++)
      result.append(lineContent.charAt(i) == '\t' ? '\t' : ' ');
    return Optional.of(result.toString());
  }

  @Override
  public String toString() {
    Optional<String> path = sourcePath();
    if (path.isPresent())
      return path.get() + ":" + line().orElse(-1).toString();
    else
      return "";
  }
  
  @Override
  public Optional<Integer> startOffset() {
    if (src.content().length == 0)
      return Optional.empty();
    else
      return Optional.of(pos.start());
  }

  @Override
  public Optional<Integer> endOffset() {
    if (src.content().length == 0)
      return Optional.empty();
    else
      return Optional.of(pos.end());
  }

  @Override
  public Optional<Integer> startLine() {
    if (src.content().length == 0)
      return Optional.empty();
    else
      return Optional.of(pos.startLine() + 1);
  }

  @Override
  public Optional<Integer> endLine() {
    if (src.content().length == 0)
      return Optional.empty();
    else
      return Optional.of(pos.endLine() + 1);
  }

  @Override
  public Optional<Integer> startColumn() {
    if (src.content().length == 0)
      return Optional.empty();
    else
      return Optional.of(pos.startColumn());
  }

  @Override
  public Optional<Integer> endColumn() {
    if (src.content().length == 0)
      return Optional.empty();
    else
      return Optional.of(pos.endColumn());
  }

}
