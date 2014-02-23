The dotty compiler frontend has been developed since November 2012 by Martin Odersky. It is expected and hoped for 
that the list of contributors to the codebase will grow quickly. Dotty draws inspiration and code from the original
Scala compiler "nsc", which is developed at [scala/scala](https://github.com/scala/scala). 

The majority of the dotty codebase is new code, with the exception of the components listed below. We have for each component tried to 
come up with a list of the original authors in the  [scala/scala](https://github.com/scala/scala) codebase. Apologies if some major authors were omitted
by oversight.

`dotty.tools.dotc.ast`

The syntax tree handling is mostly new, but some elements, such as the idea of tree copiers and the `TreeInfo` module, were adopted from  [scala/scala](https://github.com/scala/scala). 
The original authors of these parts included Martin Odersky, Paul Phillips and Matthias Zenger.

`dotty.tools.dotc.config`

> The configuration components were adapted and extended from  [scala/scala](https://github.com/scala/scala). 
> The original sources were authored by Paul Phillips with contributions from Martin Odersky, Miguel Garcia and others.
  
`dotty.tools.dotc.core`

> The core data structures and operations are mostly new. Some parts (e.g. those dealing with names) were adapted from  [scala/scala](https://github.com/scala/scala). 
> These were originally authored by Martin Odersky, Adriaan Moors, Jason Zaugg, Paul Phillips, Eugene Burmako and others.

`dotty.tools.dotc.core.pickling`

> The classfile readers were adapted from the current Scala compiler. Original authors were Martin Odersky, Iulian Dragos, Matthias Zenger and others.

`dotty.tools.dotc.parsing`

> The lexical and syntactic analysis components were adapted from the current Scala compiler. They were originally authored by Martin Odersky,
> Burak Emir, Paul Phillips, Adriaan Moors, Lex Spoon, Sean McDermid and others.

`dotty.tools.dotc.reporting`

> Adapted from  [scala/scala](https://github.com/scala/scala) with some heavy modifications. They were originally authored by Matthias Zenger, Martin Odersky, 
and others.

`dotty.tools.dotc.typer`

> Some minor components (e.g. the ConstantFolder) were adapted from the current Scala compiler. The rest is new.

`dotty.tools.dotc.util`

> The utilities package is a mix of new and adapted components. The files in  [scala/scala](https://github.com/scala/scala) were originally authored by many people,
> including Paul Phillips, Martin Odersky, Sean McDermid, and others.
  
`dotty.tools.io`   

> The I/O support library was adapted from current Scala compiler. Original authors were Paul Phillips and others.


