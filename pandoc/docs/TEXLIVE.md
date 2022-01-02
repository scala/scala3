# <span id="top">More about TeX Live</span> <span style="size:25%;"><a href="../README.md" title="Back to README">↩</a></span>
<!-- created by mics (https://github.com/michelou/) on December 2020 -->

The [TeX Live][tex_live] software is free and available on Linux, MacOS and MS Windows.

Our first task is to set up a *minimal* [TeX Live][tex_live]  installation providing the needed PDF engine for [Pandoc], namely [`lualatex`][lualatex] or [`xelatex`][xelatex]. The [TeX Live][tex_live] installer (see [GitHub repository][tex_live_repository]) offers five installation schemes :

<table style="max-width:600px;">
<tr style="color:#aaaaaa;">
<td style="text-align:right;">7135 MB</td><td>full</td><td>everything</td>
</tr>
<tr>
<td style="text-align:right;">1647 MB</td><td>medium</td><td>small + more packages and languages</td>
</tr>
<tr>
<td style="text-align:right;">524 MB</td><td>small</td><td>basic + xetex, metapost, a few languages</td>
</tr>
<tr>
<td style="text-align:right;">250 MB</td><td>basic</td><td>plain and latex</td>
</tr>
<tr style="color:#aaaaaa;">
<td style="text-align:right;">73 MB</td><td>minimal</td><td>&nbsp;<td>
</tr>
</table>

We eliminate both the *minimal* and *full* schemes straight away and choose the *basic* scheme which fits best our needs.

In the next sections we describe our setup of [TeX Live 2020][tex_live] in the MS Windows 10, Ubuntu 18.04 and Docker environments:
- [MS Windows 10](#windows) <sup>(*)</sup>
- [Ubuntu 18.04](#ubuntu) (Bionic)
- [Docker](#docker) (Ubuntu)
<div style="font-size:90%;margin:-10px 0 0 24px;"><sup>(*)</sup> Previous versions not tested (tested Windows 10 environments : <a href="https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/windows-commands#command-shell-overview">Windows Command Prompt</a> and MSYS2 Bash Shell).</div>

## <span id="windows">Setting up TeX Live 2021 on MS Windows 10</span> 

We start with the following two steps :

1. We download the setup archive [`install-tl.zip`][tex_live_setup] from the [TUG website][tex_live_tug] and extract the folder `install-tl-20210121\` (where suffix `YYYYMMDD` corresponds to the download date).
2. We run the [`install-tl-windows.bat --no-gui`][tex_live_install] command  and set the following installation options:
   <table style="max-width:650px;">
   <tr>
   <td>Scheme</td><td><code>d ☒ basic scheme</code></td><td>250 MB</td>
   </tr>
   <tr>
   <td>Collections</td><td><code>a+f+D+G+N ☒</code><sup>(*)</sup></td><td>547 MB</td>
   </tr>
   <tr>
   <td>Directories</td><td><code>c:/opt/texlive/2020</code></td><td>Default: <code>c:/texlive/2020</code></td>
   </tr>
   <tr>
   <td>Options</td><td><code>D+S+L ☐</code> and <code>M+N [None]</code></td><td>356 MB</td>
   </tr>
   </table>
   <div style="font-size:80%;"><sup>(*)</sup> <code>D</code>=LaTeX fundamental packages, <code>G</code>=LuaTeX packages, <code>N</code>=Windows-only support programs.</div>

   **NB.** The [TeX Live][tex_live] installer also creates the directory `c:\opt\texlive\texmf-local\` where private TeX packages can be installed depending on the selected TeX template.

As we run [`md2pdf.bat`](../md2pdf.bat) for the first time we get several error messages reporting missing TeX packages (available from the [TeX Live Archive](https://texlive.info/tlnet-archive/2021/01/20/tlnet/archive/)). From our usage of [`data/templates/template.tex`](../data/templates/template.tex) we have come up with the following additional TeX packages : `environ.tar.xz`, `fancyvrb.tar.xz`, `fontspec.tar.xz`, `gnu-freefont.tar.xz`, `l3backend.tar.xz`, `l3kernel.tar.xz`, `l3packages.tar.xz`, `pgf.tar.xz`, `tcolorbox.tar.xz`, `trimspaces.tar.xz`, `unicode-math.tar.xz`, `xcolor.tar.xz`.

> **:mag_right:** We use the batch file [`extras\install-tex-fonts.bat`](../extras/install-tex-fonts.bat) to download and install the additional TeX fonts (database indices updated with `texhash`). 

We can now do a quick check of our LaTeX installation :

<pre style="font-size:90%; max-width:600px;">
<b>&gt; <a href="https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/ver">ver</a> | findstr version</b>
Microsoft Windows [version 10.0.18363.1316]
&nbsp;
<b>&gt; <a href="https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/where">where</a> /r %TEXLIVE_HOME%\bin *latex.exe</b>
c:\opt\texlive\2021\bin\win32\dvilualatex.exe
c:\opt\texlive\2021\bin\win32\latex.exe
c:\opt\texlive\2021\bin\win32\lualatex.exe
c:\opt\texlive\2021\bin\win32\pdflatex.exe
&nbsp;
<b>&gt; %TEXLIVE_HOME%\bin\win32\lualatex.exe --version | findstr Version</b>
This is LuaHBTeX, Version 1.13.2 (TeX Live 2020/W32TeX)
&nbsp;
<b>&gt; <a href="https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/where">where</a> /r %PANDOC_HOME% *.exe</b>
c:\opt\pandoc-2.16.2\pandoc.exe
</pre>

> **:mag_right:** One may be curious about the directory sizes after performing the above installation steps :
> <pre style="font-size:90%; max-width:584px;">
> <b>&gt; powershell -c "function f{param($path);$n=[math]::Round((ls -r $path | measure -sum Length).sum/1024/1024);\"{0,4}M $path\" -f $n};f('c:\opt\texlive\2021');f('c:\opt\texlive\texmf-local')"</b>
> 354M c:\opt\texlive\2021
>  48M c:\opt\texlive\texmf-local
> </pre>

## <span id="ubuntu">Setting up TeX Live 2020 on Ubuntu 18.04</span>

On Ubuntu we follow the same steps as for MS Windows 10 :

1. We download the setup archive [`install-tl-unx.tar.gz`][tex_live_setup] from the [TUG website](https://tug.org/) and extract the folder `install-tl-20210121\` (where suffix `YYYYMMDD` corresponds to the download date).
2. We run the [`install-tl`][tex_live_install] command  and set the following installation options :
   <table style="max-width:650px;">
   <tr>
   <td>Scheme</td><td><code>d ☒ basic scheme</code></td><td>250 MB</td>
   </tr>
   <tr>
   <td>Collections</td><td><code>a+f+D+G ☒</code><sup>(*)</sup></td><td>423 MB</td>
   </tr>
   <tr>
   <td>Directories</td><td><code>/opt/texlive/2020</code></td><td>Default: <code>/usr/local/texlive/2020</code></td>
   </tr>
   <tr>
   <td>Options</td><td><code>D+S ☐</code> and <code>M+N [None]</code></td><td>233 MB</td>
   </tr>
   </table>
   <div style="font-size:80%;"><sup>(*)</sup> <code>D</code>=LaTeX fundamental packages, <code>G</code>=LuaTeX packages.</div>

   **NB.** The [TeX Live][tex_live] installer also creates the directory `/opt/texlive/texmf-local/` where private TeX packages can be installed depending on the selected TeX template.

> **:mag_right:** We rely on the bash script [`extras\install-tex-fonts`](../extras/install-tex-fonts) to download and install the additional TeX fonts (database indices updated with `texhash`) .  

We can now do a quick check of our LaTeX installation :

<pre style="font-size:90%; max-width:600px;">
<b>$ <a href="https://man7.org/linux/man-pages/man1/uname.1.html">uname</a> -ior</b>
4.4.0-18362-Microsoft x86_64 GNU/Linux
&nbsp;
<b>$ <a href="https://man7.org/linux/man-pages/man1/find.1.html">find</a> $TEXLIVE_HOME/bin -name *latex</b>
/opt/texlive/2021/bin/x86_64-linux/dvilualatex
/opt/texlive/2021/bin/x86_64-linux/latex
/opt/texlive/2021/bin/x86_64-linux/lualatex
/opt/texlive/2021/bin/x86_64-linux/pdflatex
&nbsp;
<b>$ /opt/texlive/2021//bin/x86_64-linux/lualatex --version | grep Version</b>
This is LuaHBTeX, Version 1.13.2 (TeX Live 2020)
</pre>

> **:mag_right:** One may be curious about the directory sizes after performing the above installation steps :
> <pre style="font-size:90%; max-width:584px;">
> <b>$ du -sh /opt/texlive/2020 /opt/texlive/texmf-local/</b>
> 246M    /opt/texlive/2020
> 25M     /opt/texlive/texmf-local/
> </pre>

## <span id="docker">Setting up TeX Live 2020 on Docker</span>

We use [`pandoc/ubuntu-latex`][docker_pandoc], a Ubuntu based image which bundles [TeX Live][tex_live] and [Pandoc]; the Docker image is available from the [Docker Hub][docker_hub].

> See document [`DOCKER.md`](./DOCKER.md) for further information, e.g. `Dockerfile` usage.

<!--
## <span id="footnotes">Footnotes</span>

<span id="footnote_01">[1]</span> ***Pandoc software*** [↩](#anchor_01)

<p style="margin:0 0 1em 20px;">
 (version: 2.11.3.2, compressed size: 240.96 MB)
</p>
-->
***

*[mics](https://github.com/michelou/)/January 2022* [**&#9650;**](#top "Back to top")
<span id="bottom">&nbsp;</span>

[docker_entrypoint]: https://docs.docker.com/engine/reference/builder/#entrypoint "ENTRYPOINT instruction"
[docker_env]: https://docs.docker.com/engine/reference/builder/#env
[docker_hub]: https://hub.docker.com/
[docker_pandoc]: https://hub.docker.com/r/pandoc/ubuntu-latex
[lualatex]: http://www.luatex.org/
[pandoc]: https://pandoc.org/ "A universal document converter"
[tex_live]: https://tug.org/texlive/
[tex_live_repository]: https://github.com/TeX-Live/installer "TeX Live installer repository"
[tex_live_install]: https://tug.org/texlive/doc/install-tl.html "TeX Live cross-platform installer"
[tex_live_setup]: https://tug.org/texlive/acquire-netinstall.html
[tex_live_tug]: https://tug.org/ "TeX Users Group website"
[xelatex]: https://sourceforge.net/projects/xetex/
