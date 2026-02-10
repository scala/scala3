/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package xsbt;

import xsbti.*;
import xsbti.compile.*;

import java.io.File;
import java.util.Arrays;
import java.util.HashMap;

import dotty.tools.dotc.core.Contexts.Context;
import dotty.tools.dotc.core.Contexts.ContextBase;
import dotty.tools.dotc.Main;
import dotty.tools.io.AbstractFile;
import dotty.tools.xsbt.InterfaceCompileFailed;
import dotty.tools.xsbt.DelegatingReporter;
import dotty.tools.xsbt.OldIncrementalCallback;

import dotty.tools.dotc.sbt.interfaces.IncrementalCallback;

import scala.collection.mutable.ListBuffer;

// deprecation warnings are suppressed because scala3-sbt-bridge must stay compatible with Zinc 1.3
// see https://github.com/scala/scala3/issues/10816
@SuppressWarnings("deprecation")
public class CachedCompilerImpl implements CachedCompiler {
  private final String[] args;
  private final String[] outputArgs;

  public CachedCompilerImpl(String[] args, Output output) {
    super();
    this.args = args;

    if (!(output instanceof SingleOutput))
      throw new IllegalArgumentException("output should be a SingleOutput, was a " + output.getClass().getName());

    this.outputArgs =
      new String[] { "-d", ((SingleOutput) output).getOutputDirectory().getAbsolutePath() };
  }

  public String[] commandArguments(File[] sources) {
    String[] sortedSourcesAbsolute = new String[sources.length];
    for (int i = 0; i < sources.length; i++)
      sortedSourcesAbsolute[i] = sources[i].getAbsolutePath();
    java.util.Arrays.sort(sortedSourcesAbsolute);

    // Concatenate outputArgs, args and sortedSourcesAbsolute
    String[] result = new String[outputArgs.length + args.length + sortedSourcesAbsolute.length];
    int j = 0;
    for (int i = 0; i < outputArgs.length; i++, j++)
      result[j] = outputArgs[i];
    for (int i = 0; i < args.length; i++, j++)
      result[j] = args[i];
    for (int i = 0; i < sortedSourcesAbsolute.length; i++, j++)
      result[j] = sortedSourcesAbsolute[i];

    return result;
  }

  synchronized public void run(File[] sources, DependencyChanges changes, AnalysisCallback callback, Logger log,
    Reporter delegate, CompileProgress progress) {
    log.debug(() -> {
      String msg = "Calling Dotty compiler with arguments  (CompilerInterface):";
      for (String arg : args)
        msg = msg + "\n\t" + arg;
      return msg;
    });

    ListBuffer<AbstractFile> sourcesBuffer = new ListBuffer<>();
    HashMap<AbstractFile, VirtualFile> lookup = new HashMap<>(sources.length, 0.25f);

    VirtualFile[] sortedSources = new VirtualFile[sources.length];
    for (int i = 0; i < sources.length; i++) {
      final int i0 = i;
      sortedSources[i] = new PathBasedFile() {
        public java.nio.file.Path toPath() {
          return sources[i0].toPath();
        }
        public java.io.InputStream input() {
          try {
            return java.nio.file.Files.newInputStream(toPath());
          } catch (java.io.IOException ex) {
            throw new RuntimeException(ex);
          }
        }

        public long contentHash() {
          try {
            int murmurHash3 = scala.util.hashing.MurmurHash3$.MODULE$.bytesHash(java.nio.file.Files.readAllBytes(toPath()));
            return (long) murmurHash3;
          } catch (java.io.IOException ex) {
            throw new RuntimeException(ex);
          }
        }

        public String id() {
          return toPath().toString();
        }
        public String name() {
          return toPath().getFileName().toString();
        }
        public String[] names() {
          return new String[] { name() };
        }
      };
    }
    Arrays.sort(sortedSources, (x0, x1) -> x0.id().compareTo(x1.id()));

    for (int i = 0; i < sources.length; i++) {
      VirtualFile source = sortedSources[i];
      AbstractFile abstractFile = dotty.tools.xsbt.CompilerBridgeDriver.asDottyFile(source);
      sourcesBuffer.append(abstractFile);
      lookup.put(abstractFile, source);
    }

    DelegatingReporter reporter0 = new DelegatingReporter(delegate, sourceFile -> {
      // TODO: possible situation here where we use -from-tasty and TASTy source files but
      // the reporter log is associated to a Scala source file?

      // Zinc will use the output of this function to possibly lookup a mapped virtual file,
      // e.g. convert `${ROOT}/Foo.scala` to `/path/to/Foo.scala` if it exists in the lookup map.
      VirtualFile vf = lookup.get(sourceFile.file());
      if (vf != null)
        return vf.id();
      else
        // follow Zinc, which uses the path of the source file as a fallback.
        return sourceFile.path();
    });

    IncrementalCallback incCallback = new dotty.tools.xsbt.IncrementalCallback(callback, sourceFile ->
      dotty.tools.xsbt.CompilerBridgeDriver.asVirtualFile(sourceFile, reporter0, lookup)
    );

    Context ctx = new ContextBase().initialCtx().fresh()
      .setIncCallback(incCallback)
      .setReporter(new DelegatingReporter(delegate, source -> source.file().absolutePath()));

    dotty.tools.dotc.reporting.Reporter reporter = Main.process(commandArguments(sources), ctx);
    if (reporter.hasErrors()) {
      throw new InterfaceCompileFailed(args, new Problem[0], "Compilation failed");
    }
  }
}
