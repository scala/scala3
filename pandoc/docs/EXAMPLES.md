# <span id="top">Usage examples</span> <span style="size:25%;"><a href="../README.md" title="Back to README">↩</a></span>
<!-- created by mics (https://github.com/michelou/) on December 2020 -->

In the following we illustrate the usage of the two scripts listed in document [`PROJECT.md`](./PROJECT.md "More about the Pandoc project").

### <span id="bash">Bash Shell</span>

This section presents examples in a Unix-like environment, e.g. [MSYS2] or [Ubuntu].

> **:mag_right:** For the sake of simplicity we assume that the following variables are defined in the execution environment :
> <pre style="max-width:585px;">
> <b>export</b> <b style="color:darkred;">PANDOC_HOME</b>=/opt/pandoc    (<i>symlink</i> pandoc <b>-></b> pandoc-2.16.2)
> <b>export</b> <b style="color:darkred;">TEXLIVE_HOME</b>=/opt/texlive/2020
> </pre>
> or in a <a href="https://www.msys2.org/">MSYS2</a> environment :
> <pre style="max-width:585px;">
> $ <b>export</b> <b style="color:darkred;">GIT_HOME</b>=/c/opt/Git-2.34.1/
> $ <b>export</b> <b style="color:darkred;">PANDOC_HOME</b>=/c/opt/pandoc-2.16.2/
> $ <b>export</b> <b style="color:darkred;">TEXLIVE_HOME</b>=/c/opt/texlive/2021/
> </pre>

Command [`./build`](../build) generates the PDF file `scala3_reference.pdf` :

<pre style="max-width:600px;">
<b>pandoc$ ./<a href="../build">build</a></b>
Elapsed time: 59 seconds
&nbsp;
<b>pandoc$ <a href="https://man7.org/linux/man-pages/man1/ls.1.html">ls</a> ../out/pandoc</b>
images  scala3_reference.pdf  src_managed
</pre>

With `DEBUG=1` we execute [`md2pdf`](../md2pdf) in debug mode :

<pre>
<b>pandoc$ DEBUG=1 ./<a href="../md2pdf">md2pdf</a></b>
Copy image files to directory "/tmp/dotty/out/pandoc/images"
Preprocess file "/tmp/dotty/docs/docs/reference/changed-features/compiler-plugins.md"
[...]
Preprocess file "/tmp/dotty/docs/docs/reference/syntax.md"
/opt/pandoc/bin/pandoc <b style="color:darkred;">--data-dir</b>=/tmp/dotty/pandoc/data <b style="color:darkred;">--defaults</b>=/tmp/dotty/pandoc/data/defaults.yaml <b style="color:darkred;">--syntax-definition</b>=/tmp/dotty/pandoc/data/templates/scala.xml -V geometry=a4paper -V geometry:margin=30mm -V mainfont:FreeSerif.ttf -V fontsize=12pt -V urlcolor=blue -V linkcolor=[HTML]{66001E} -V subtitle='Internal Draft (rev 82a8762c24)' -V date='2 January 2021' <b style="color:darkred;">--template</b>=/tmp/dotty/pandoc/data/templates/template.tex <b style="color:darkred;">--pdf-engine</b>=/opt/texlive/2021//bin/x86_64-linux/lualatex <b style="color:darkred;">--output</b>=/tmp/dotty/out/pandoc/scala3_reference.pdf /tmp/dotty/pandoc/data/reference.md
&nbsp;
<b>pandoc$ <a href="https://man7.org/linux/man-pages/man1/ls.1.html">ls</a> ../out/pandoc/</b>
images  scala3_reference.pdf  src_managed
</pre>

> **:mag_right:** The preprocessing step converts cross-reference between [Markdown] documents into [named anchors][named_anchor]. The transformed files are stored into the output directory `../out/pandoc/src_managed/` and are then passed to [`pandoc`][pandoc_cmd].

### <span id="windows">Windows Command Prompt</span>  <sup style="font-size:60%;">[**&#9650;**](#top "Back to top")</sup>

This section presents examples in a MS Windows environment, e.g. [MS Windows 10 Pro][win_10_pro].

> **:mag_right:** For the sake of simplicity we assume that the following environment variables are defined :
> <pre style="max-width:585px;">
> <b>set</b> <b style="color:darkred;">GIT_HOME</b>=C:\opt\Git-2.34.1
> <b>set</b> <b style="color:darkred;">PANDOC_HOME</b>=C:\opt\pandoc-2.16.2
> <b>set</b> <b style="color:darkred;">TEXLIVE_HOME</b>=C:\opt\texlive\2021
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

> **:mag_right:** Use the command [`build help`](../build) (resp. [`build.bat help`](../build.bat)) to display the available options/subcommands.

We can specify option `-project:internals` to produce the output file `scala3_internals.pdf` :

<pre style="max-width:640px;">
<b>pandoc&gt; <a href="../build.bat">build</a> -timer -debug -project:internals clean compile</b>
[build] Options    : _TIMER=1 _VERBOSE=0
[build] Subcommands: _CLEAN=1 _COMPILE=1 _RUN=0
[build] Variables  : <b style="color:darkred;">PANDOC_HOME</b>=c:\opt\pandoc-2.16.2
[build] Variables  : <b style="color:darkred;">TEXLIVE_HOME</b>=c:\opt\texlive\2021
[build] Variables  : _PROJECT_NAME=internals
[build] rmdir /s /q "W:\scala3-pandoc\out\pandoc"
[build] "W:\scala3-pandoc\pandoc\md2pdf.bat" "internals"
Total elapsed time: 00:00:38
[build] _EXITCODE=0
&nbsp;
<b>pandoc&gt; ls -l ..\out\pandoc | sed "s/%USERNAME%//"</b>
total 188
drwxr-xr-x 1  197121      0 Jan 30 19:29 images
-rw-r--r-- 1  197121 185157 Jan 30 19:30 scala3_internals.pdf
drwxr-xr-x 1  197121      0 Jan 30 19:29 src_managed
</pre>

***

*[mics](https://github.com/michelou/)/January 2022* [**&#9650;**](#top "Back to top")
<span id="bottom">&nbsp;</span>

[markdown]: https://commonmark.org/
[msys2]: https://www.msys2.org/
[named_anchor]: https://stackoverflow.com/questions/5319754/cross-reference-named-anchor-in-markdown
[pandoc_cmd]: https://pandoc.org/MANUAL.html
[ubuntu]: https://ubuntu.com/desktop
[win_10_pro]: https://www.microsoft.com/en-us/windows/compare-windows-10-home-vs-pro
