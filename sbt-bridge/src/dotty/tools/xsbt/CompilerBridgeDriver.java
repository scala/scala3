/*
 * Zinc - The incremental compiler for Scala.
 * Copyright Lightbend, Inc. and Mark Harrah
 */

package dotty.tools.xsbt;

import dotty.tools.dotc.Compiler;
import dotty.tools.dotc.Driver;
import dotty.tools.dotc.ScalacCommand;
import dotty.tools.dotc.config.Properties;
import dotty.tools.dotc.core.Contexts;
import dotty.tools.dotc.util.SourceFile;
import dotty.tools.io.AbstractFile;
import scala.collection.mutable.ListBuffer;
import scala.jdk.javaapi.CollectionConverters;
import scala.io.Codec;
import xsbti.Problem;
import xsbti.*;
import xsbti.compile.Output;

import java.io.IOException;
import java.util.Comparator;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Arrays;

public class CompilerBridgeDriver extends Driver {
  private final String[] scalacOptions;
  private final String[] args;

  public CompilerBridgeDriver(String[] scalacOptions, Output output) {
    super();
    this.scalacOptions = scalacOptions;

    if (!output.getSingleOutputAsPath().isPresent())
      throw new IllegalArgumentException("output should be a SingleOutput, was a " + output.getClass().getName());

    this.args = new String[scalacOptions.length + 2];
    System.arraycopy(scalacOptions, 0, args, 0, scalacOptions.length);
    args[scalacOptions.length] = "-d";
    args[scalacOptions.length + 1] = output.getSingleOutputAsPath().get().toAbsolutePath().toString();
  }

  private static final String StopInfoError =
    "Compiler option supplied that disabled Zinc compilation.";

  /**
   * `sourcesRequired` is set to false because the context is set up with no sources
   * The sources are passed programmatically to the compiler in the form of AbstractFiles
   */
  @Override
  public boolean sourcesRequired() {
    return false;
  }

  private static VirtualFile asVirtualFile(SourceFile sourceFile, DelegatingReporter reporter,
      HashMap<AbstractFile, VirtualFile> lookup) {
    return lookup.computeIfAbsent(sourceFile.file(), path -> {
      reportMissingFile(reporter, sourceFile);
      if (sourceFile.file().jpath() != null)
        return new BasicPathBasedFile(sourceFile);
      else
        return new PlaceholderVirtualFile(sourceFile);
    });
  }

  private static void reportMissingFile(DelegatingReporter reporter, SourceFile sourceFile) {
    String underline = String.join("", Collections.nCopies(sourceFile.path().length(), "^"));
    String message =
      sourceFile.path() + ": Missing virtual file\n" +
      underline + "\n" +
      "    Falling back to placeholder for the given source file (of class " + sourceFile.getClass().getName() + ")\n" +
      "    This is likely a bug in incremental compilation for the Scala 3 compiler. Please report it to the Scala 3 maintainers.";
    reporter.reportBasicWarning(message);
  }

  synchronized public void run(VirtualFile[] sources, AnalysisCallback callback, Logger log, Reporter delegate) {
    VirtualFile[] sortedSources = new VirtualFile[sources.length];
    System.arraycopy(sources, 0, sortedSources, 0, sources.length);
    Arrays.sort(sortedSources, (x0, x1) -> x0.id().compareTo(x1.id()));

    ListBuffer<AbstractFile> sourcesBuffer = new ListBuffer<>();
    HashMap<AbstractFile, VirtualFile> lookup = new HashMap<>(sources.length, 0.25f);

    for (int i = 0; i < sources.length; i++) {
      VirtualFile source = sortedSources[i];
      AbstractFile abstractFile = asDottyFile(source);
      sourcesBuffer.append(abstractFile);
      lookup.put(abstractFile, source);
    }

    DelegatingReporter reporter = new DelegatingReporter(delegate, (self, sourceFile) ->
      asVirtualFile(sourceFile, self, lookup).id()
    );

    IncrementalCallback incCallback = new IncrementalCallback(callback, sourceFile ->
      asVirtualFile(sourceFile, reporter, lookup)
    );

    try {
      log.debug(this::infoOnCachedCompiler);

      Contexts.Context initialCtx = initCtx()
        .fresh()
        .setReporter(reporter)
        .setIncCallback(incCallback);

      Contexts.Context context = setup(args, initialCtx).map(t -> t._2).getOrElse(() -> initialCtx);

      if (ScalacCommand.isHelpFlag(context.settings(), context.settingsState())) {
        throw new InterfaceCompileFailed(args, new Problem[0], StopInfoError);
      }

      if (!delegate.hasErrors()) {
        log.debug(this::prettyPrintCompilationArguments);
        Compiler compiler = newCompiler(context);

        doCompile(compiler, sourcesBuffer.toList(), context);

        for (xsbti.Problem problem: delegate.problems()) {
          callback.problem(problem.category(), problem.position(), problem.message(), problem.severity(),
            true);
        }
      } else {
        delegate.printSummary();
      }

      if (delegate.hasErrors()) {
        log.debug(() -> "Compilation failed");
        throw new InterfaceCompileFailed(args, delegate.problems(), "Compilation failed");
      }
    } finally {
      reporter.dropDelegate();
    }
  }

  private static AbstractFile asDottyFile(VirtualFile virtualFile) {
    if (virtualFile instanceof PathBasedFile)
      return new ZincPlainFile((PathBasedFile) virtualFile);

    try {
      return new ZincVirtualFile(virtualFile);
    } catch (IOException e) {
      throw new IllegalArgumentException("invalid file " + virtualFile.name(), e);
    }
  }

  private String infoOnCachedCompiler() {
    String compilerId = Integer.toHexString(hashCode());
    String compilerVersion = Properties.versionString();
    return String.format("[zinc] Running cached compiler %s for Scala Compiler %s", compilerId, compilerVersion);
  }

  private String prettyPrintCompilationArguments() {
    StringBuilder builder = new StringBuilder();
    builder.append("[zinc] The Scala compiler is invoked with:");
    for (String opt: scalacOptions) {
      builder.append("\n\t");
      builder.append(opt);
    }
    return builder.toString();
  }

}
