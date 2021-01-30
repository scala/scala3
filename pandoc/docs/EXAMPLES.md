# <span id="top">Usage examples</span> <span style="size:25%;"><a href="../README.md" title="Back to README">↩</a></span>
<!-- created by mics (https://github.com/michelou/) on December 2020 -->

In the following we illustrate the usage of the two scripts listed in document [`PROJECT.md`](./PROJECT.md "More about the Pandoc project").

### <span id="bash">Bash Shell</span>

This section presents examples in a Unix-like environment, e.g. [MSYS2] or [Ubuntu].

> **:mag_right:** For the sake of simplicity we assume that the following variables are defined in the execution environment :
> <pre style="max-width:585px;">
> <b>export</b> <b style="color:darkred;">PANDOC_HOME</b>=/opt/pandoc    (<i>symlink</i> pandoc <b>-></b> pandoc-2.11.4)
> <b>export</b> <b style="color:darkred;">TEXLIVE_HOME</b>=/opt/texlive/2020
> </pre>
> or in a <a href="https://www.msys2.org/">MSYS2</a> environment :
> <pre style="max-width:585px;">
> $ <b>export</b> <b style="color:darkred;">GIT_HOME</b>=/c/opt/Git-2.30.0/
> $ <b>export</b> <b style="color:darkred;">PANDOC_HOME</b>=/c/opt/pandoc-2.11.4/
> $ <b>export</b> <b style="color:darkred;">TEXLIVE_HOME</b>=/c/opt/texlive/2020/
> </pre>

Command [`./build`](../build) generates the PDF file `scala3_reference.pdf` :

<pre style="max-width:600px;">
<b>pandoc$ ./<a href="../build">build</a></b>
Elapsed time: 59 seconds
&nbsp;
<b>pandoc$ <a href="https://man7.org/linux/man-pages/man1/ls.1.html">ls</a> ../out/pandoc</b>
images  scala3_reference.pdf  src_managed
</pre>

With `DEBUG=1` we execute [`md2pd`](../md2pdf) in debug mode :

<pre>
<b>pandoc$ DEBUG=1 ./<a href="../md2pdf">md2pdf</a></b>
Copy image files to directory "/tmp/dotty/out/pandoc/images"
Preprocess file "/tmp/dotty/docs/docs/reference/changed-features/compiler-plugins.md"
[...]
Preprocess file "/tmp/dotty/docs/docs/reference/syntax.md"
/opt/pandoc/bin/pandoc <b style="color:darkred;">--data-dir</b>=/tmp/dotty/pandoc/data <b style="color:darkred;">--defaults</b>=/tmp/dotty/pandoc/data/defaults.yaml <b style="color:darkred;">--syntax-definition</b>=/tmp/dotty/pandoc/data/templates/scala.xml -V geometry=a4paper -V geometry:margin=30mm -V mainfont:FreeSerif.ttf -V fontsize=12pt -V urlcolor=blue -V linkcolor=[HTML]{66001E} -V subtitle='Internal Draft (rev 82a8762c24)' -V date='22 January 2021' <b style="color:darkred;">--template</b>=/tmp/dotty/pandoc/data/templates/template.tex <b style="color:darkred;">--pdf-engine</b>=/opt/texlive/2020//bin/x86_64-linux/lualatex <b style="color:darkred;">--output</b>=/tmp/dotty/out/pandoc/scala3_reference.pdf /tmp/dotty/pandoc/data/reference.md
&nbsp;
<b>pandoc$ <a href="https://man7.org/linux/man-pages/man1/ls.1.html">ls</a> ../out/pandoc/</b>
images  scala3_reference.pdf  src_managed
</pre>

> **:mag_right:** The preprocessing step converts cross reference between Markdown documents into URL anchors. The transformed files are stored into the output directory `../out/pandoc/src_managed/` and are then passed to <a href="https://pandoc.org/"><code>pandoc</code></a>.

### <span id="windows">Windows Command Prompt</span>  <sup style="font-size:60%;">[**&#9650;**](#top "Back to top")</sup>

This section presents examples in a MS Windows environment, e.g. [MS Windows 10 Pro][win_10_pro].

> **:mag_right:** For the sake of simplicity we assume that the following environment variables are defined :
> <pre style="max-width:585px;">
> <b>set</b> <b style="color:darkred;">GIT_HOME</b>=C:\opt\Git-2.30.0
> <b>set</b> <b style="color:darkred;">PANDOC_HOME</b>=C:\opt\pandoc-2.11.4
> <b>set</b> <b style="color:darkred;">TEXLIVE_HOME</b>=C:\opt\texlive\2020
> </pre>

Command [`build.bat`](../build.bat) generates the PDF file `scala3_reference.pdf`; with option `-timer` it prints the elapsed time and with option `-verbose` it also prints progress messages :

<pre style="max-width:600px;">
<b>pandoc&gt; <a href="../build.bat">build</a> -timer -verbose clean compile</b>
Delete directory "W:\scala3-pandoc\out\pandoc"
md2pdf.bat "reference"
Total elapsed time: 00:01:17
&nbsp;
<b>pandoc&gt; <a href="https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/dir">dir</a> /b ..\out\pandoc</b>
images
scala3_reference.pdf
src_managed
</pre>

Currently the batch file [`build.bat`](../build.bat) offers more functionality than the bash script [`build`](../build); for instance :
- option `-project:<name>` allows us to generate a PDF file for other projects, e.g. [`internals`](https://github.com/lampepfl/dotty/tree/master/docs/docs/internals).
- subcommand `view` displays the generated PDF file using the default PDF viewer.

<pre style="max-width:600px;">
<b>pandoc&gt; <a href="../build.bat">build</a></b>
Usage: build { &lt;option&gt; | &lt;subcommand&gt; }
&nbsp;
  Options:
    -debug           show commands executed by this script
    -project:&lt;name&gt;  project name (default: <a href="https://github.com/lampepfl/dotty/tree/master/docs/docs/reference">reference</a>)
    -verbose         display progress messages
&nbsp;
  Subcommands:
    clean            delete generated class files
    compile          compile source files
    run              open generated PDF file in default application
    view             alias for run
</pre>

***

*[mics](https://github.com/michelou/)/January 2021* [**&#9650;**](#top "Back to top")
<span id="bottom">&nbsp;</span>

[msys2]: https://www.msys2.org/
[ubuntu]: https://ubuntu.com/desktop
[win_10_pro]: https://www.microsoft.com/en-us/windows/compare-windows-10-home-vs-pro
