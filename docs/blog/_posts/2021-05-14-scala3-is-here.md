---
layout: blog-page
title: Scala 3 is here!🎉🎉🎉
author: Anatolii Kmetiuk
authorImg: /images/anatolii.png
date: 2021-05-14
---

<iframe width="560" height="315" src="https://www.youtube.com/embed/0yRAtLL18cY" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

<!--more-->

After 8 years of work, 28,000 commits, 7,400 pull requests, 4,100 closed issues – Scala 3 is finally out. Since the first commit on December 6th 2012, more than a hundred people have contributed to the project. Today, Scala 3 incorporates the latest research in type theory as well as the industry experience of Scala 2. We've seen what worked well (or not so well) for the community in Scala 2. Based on this experience we've created the third iteration of Scala – easy to use, learn, and scale.

## Exciting new features: how to get started?

A lot of new features ended up in Scala 3, and you may wonder where you should start learning about all of it. The go-to place for learning Scala 3 is its [documentation](https://docs.scala-lang.org/scala3/).

To get a taste of all the new features that went into Scala 3, you can have a look at the [New in Scala 3 article](https://docs.scala-lang.org/scala3/new-in-scala3.html). For a streamlined and more thorough introduction, see the [Scala 3 Book](https://docs.scala-lang.org/scala3/book/introduction.html). You can try Scala 3 online without installing anything on your machine via [Scastie](https://scastie.scala-lang.org/?target=dotty), or you can follow the [Getting Started guide](https://docs.scala-lang.org/scala3/getting-started.html) to install it on your machine.

One thing has changed completely in Scala 3 compared to Scala 2: it is macros. You can learn more about how they work in Scala 3 in the dedicated [documentation](https://docs.scala-lang.org/scala3/guides/macros/index.html).

If you are a seasoned user of Scala 2, chances are you have some projects you'd like to port from Scala 2 to Scala 3. You may find the [Migration guide](https://docs.scala-lang.org/scala3/guides/migration/compatibility-intro.html#) helpful. It outlines the compatibility story between Scala 2 and Scala 3: source compatibility, binary compatibility, changed and dropped features, metaprogramming.

And if you need any help in your Scala journey, you can find various resources to talk to fellow Scala users [here](https://www.scala-lang.org/community/).

## Ecosystem

Who's already on Scala 3? Which libraries can you already use? A great place to answer this question is [Scaladex](https://index.scala-lang.org/). Scaladex is an index of Scala libraries where you can explore the ecosystem by language version, platform or the type of job a library does. At the time of writing this article, there are 308 early adopter libraries on Scala 3, compared to 2597 Scala 2.13 libraries.

## Releases and Guarantees in 3.x era

We intend to continue releasing every 6 weeks after 3.0.0, bumping the patch version every time we do so. A stable 3.0.x release will be preceded with 3.0.x-RC1 release candidate 6 weeks prior to such a stable release. Such patch releases will contain bug fixes for bugs affecting the respective minor version. Patch versions will be forward and backward compatible with each other over the source, binary and TASTy compatibilities.

Of course, we intend to continue the development of the language in ways other than bug fixing. New language features and standard library APIs will appear in future minor versions. As any addition in library APIs, these may break backward source compatibility in minor, rare ways. However, minor releases will not break backward binary or TASTy compatibility. Concretely, that means that libraries built with Scala 3.0.0 will keep working with Scala 3.x.y!


## Library authors: Join our community build

Scala 3 now has a set of widely-used community libraries that are built against every nightly Scala 3
snapshot. Currently, this includes shapeless, ScalaPB, algebra, scalatest, scopt and squants.
Join our [community build](https://github.com/lampepfl/dotty/tree/master/community-build)
to make sure that our regression suite includes your library.

## Contributors

Thank you to all the contributors who made this release possible 🎉

All-time contributors who made Scala 3 a reality, according to `git shortlog -sn --no-merges 2308509d2651ee78e1122b5d61b798c984c96c4d..3.0.0`, are:

```
  8661  Martin Odersky
  3186  Nicolas Stucki
  1435  Guillaume Martres
   976  Dmitry Petrashko
   797  Liu Fengyun
   774  Felix Mulder
   407  Allan Renucci
   324  liu fengyun
   324  Olivier Blanvillain
   323  Martin Duhem
   224  Aleksander Boruch-Gruszecki
   204  Jamie Thompson
   201  Krzysztof Romanowski
   200  Sébastien Doeraene
   172  Paolo G. Giarrusso
   171  Bryan Abate
   163  Aggelos Biboudis
   162  Anatolii Kmetiuk
   160  Anatolii
   129  Robert Stoll
   103  bishabosha
   103  Filip Zybała
   101  Miles Sabin
    82  Antoine Brunner
    64  poechsel
    64  Guillaume Raffin
    62  Tom Grigg
    61  Lan, Jian
    55  noti0na1
    54  Andrzej Ratajczak
    51  odersky
    50  Nikita Eshkeev
    44  Guillaume R
    37  Stéphane Micheloud
    34  Enno Runne
    33  Sara Alemanno
    31  Paweł Marks
    30  Ondrej Lhotak
    29  Som Snytt
    29  Abel Nieto
    26  Ruslan Shevchenko
    25  VladimirNik
    23  Adrien Piquerez
    22  Raphael Jolly
    22  Jonathan Brachthäuser
    22  Michał Pałka
    20  vsalvis
    20  Tobias Bordenca
    20  Fengyun Liu
    19  Martijn Hoekstra
    19  Samuel Gruetter
    19  Phil
    19  Maxime Kjaer
    18  Jendrik Wenke
    17  Jason Zaugg
    16  Krzysztof Romanwoski
    16  Arnaud ESTEVE
    15  Dale Wijnand
    14  Jaemin Hong
    13  gzoller
    13  Vlad Ureche
    12  Miron Aseev
    12  Wojtek Swiderski
    11  Yichen Xu
    11  Grzegorz Bielski
    10  Sebastian Nadorp
    10  Jentsch
    10  bjornregnell
    10  Arnaud Esteve
    10  Dmytro Melnychenko
    10  Lionel Parreaux
     9  Jonathan Brachthäuser
     9  yu-croco
     9  Jasper Moeys
     8  Clemens Winter
     8  Lukas Rytz
     8  Varunram Ganesh
     8  Oron Port
     8  Reto Hablützel
     7  Ólafur Páll Geirsson
     7  Varunram
     7  benkobalog
     7  Eugene Melekhov
     6  jvican
     6  Seth Tisue
     6  Natsu Kagami
     6  Thierry Treyer
     6  Akhtiam Sakaev
     6  Olivier ROLAND
     6  Olafur Pall Geirsson
     5  Nada Amin
     5  Michael Pilquist
     5  Ausmarton Zarino Fernandes
     5  k0ala
     5  Vlastimil Dort
     5  Valthor Halldorsson
     5  Travis Brown
     5  Tomasz Godzik
     5  Alex Merritt
     5  Guillaume Massé
     5  Alexander Myltsev
     5  Saloni Vithalani
     5  Raphael Bosshard
     5  Julien Richard-Foy
     4  Michał Gutowski
     4  Sebastian Harko
     4  fhackett
     4  ysthakur
     4  Ben Elliott
     4  Raymond Tay
     4  Ayush
     4  Neeraj Jaiswal
     4  Sarunas Valaskevicius
     4  Lucas Burson
     4  Dotty CI
     4  Eric K Richardson
     4  Vitor Vieira
     4  Yevgen Nerush
     4  Shane Delmore
     4  Andrew Valencik
     4  senia-psm
     4  Minghao Liu
     4  Matt D'Souza
     4  Eugene Yokota
     4  Hanns Holger Rutz
     4  Alex Zolotko
     4  Georg Schmid
     4  Chris Birchall
     4  december32
     4  Ingar Abrahamsen
     3  Michal Gutowski
     3  Gabriele Petronella
     3  Gabi Volpe
     3  Master-Killer
     3  Uko
     3  Timothée Floure
     3  xuwei-k
     3  Eric Loots
     3  Enno
     3  Edmund Noble
     3  Saurabh Rawat
     3  Albert Chen
     3  Jakob Odersky
     3  Daniel Li
     3  Dani Rey
     3  ansvonwa
     3  duanebester
     3  Alexandre Archambault
     3  jerylee
     3  kenji yoshida
     3  Artur Opala
     3  Adriaan Moors
     3  Ankit Soni
     3  Adam Fraser
     3  Pavel Shirshov
     3  João Pedro Evangelista
     3  Andrea Mocci
     3  Krzysztof Bochenek
     3  Tudor Voicu
     2  Tobias Schlatter
     2  Alden Torres
     2  AnEmortalKid
     2  Andrew Zurn
     2  Ara Adkins
     2  Artsiom Miklushou
     2  Ashwin Bhaskar
     2  Aurélien Richez
     2  Camila Andrea Gonzalez Williamson
     2  Dvir Faivel
     2  Fabian Page
     2  FabioPinheiro
     2  Francois GORET
     2  Glavo
     2  Greg Pevnev
     2  Henrik Huttunen
     2  Hermes Espínola González
     2  James Thompson
     2  Jan Christopher Vogt
     2  Jens Kat
     2  Jim Van Horn
     2  Jon Pretty
     2  Lorand Szakacs
     2  Luc Henninger
     2  Lucas
     2  Matthew Pickering
     2  Matthias Sperl
     2  Mikael Blomstrand
     2  Nadezhda Balashova
     2  Nikolay
     2  Nikolay.Tropin
     2  Patrik Mada
     2  Philippus
     2  Philippus Baalman
     2  Radosław Waśko
     2  Rafal Piotrowski
     2  Robert Soeldner
     2  Roberto Bonvallet
     2  Rodrigo Fernandes
     2  Steven Heidel
     2  Thiago Pereira
     2  Tudor
     2  William Narmontas
     2  changvvb
     2  dos65
     2  esarbe
     2  johnregan
     2  lloydmeta
     2  typeness
     2  veera venky
     2  xhudik
     2  ybasket
     1  Jyotman Singh
     1  Justin du Coeur, AKA Mark Waks
     1  Julien Jean Paul Sirocchi
     1  João Pedro de Carvalho
     1  rsoeldner
     1  Jonathan Skowera
     1  Jonathan Rodriguez
     1  Jon-Anders Teigen
     1  ruben
     1  Alexander Slesarenko
     1  Pierre Ricadat
     1  Piotr Gabara
     1  squid314
     1  tOverney
     1  Raj Parekh
     1  Rajesh Veeranki
     1  John Sullivan
     1  Johannes Rudolph
     1  Joan
     1  Jimin Hsieh
     1  Richard Beddington
     1  Rick M
     1  Rike-Benjamin Schuppner
     1  tanaka takaya
     1  Jean Detoeuf
     1  tanishiking
     1  tim-zh
     1  Jarrod Janssen
     1  Jan Rock
     1  Sam Desborough
     1  Jakub Kozłowski
     1  Sandro Stucki
     1  Jacob J
     1  Jaap van der Plas
     1  Ivano Pagano
     1  Ivan Youroff
     1  Iltotore
     1  Serhii Pererva
     1  Igor Mielientiev
     1  Ignasi Marimon-Clos
     1  Simon Hafner
     1  Simon Popugaev
     1  Ian Tabolt
     1  SrTobi
     1  Stefan Zeiger
     1  Stephane MICHELOUD
     1  tokkiyaa
     1  Stéphane MICHELOUD
     1  Herdy Handoko
     1  Szymon Pajzert
     1  Harrison Houghton
     1  Taisuke Oe
     1  yytyd
     1  Harpreet Singh
     1  Haemin Yoo
     1  Timur Abishev
     1  Grzegorz Kossakowski
     1  Tobias Kahlert
     1  0xflotus
     1  Greg Zoller
     1  Tomas
     1  George Leontiev
     1  Florian Schmaus
     1  zgrybus
     1  Florian Cassayre
     1  Ferhat Aydın
     1  Umayah Abdennabi
     1  Fedor Shiriaev
     1  Dmitry Melnichenko
     1  Dmitrii Naumenko
     1  Vasil Vasilev
     1  Victor
     1  Deon Taljaard
     1  Denis Buzdalov
     1  Dean Wampler
     1  David Hoepelman
     1  Vykintas Narmontas (William)
     1  Alexander Shamukov
     1  DarkDimius
     1  Daniel Reigada
     1  Daniel Murray
     1  Yilin Wei
     1  Zoltán Elek
     1  adpi2
     1  aesteve
     1  amanjpro
     1  andreaTP
     1  Damian Albrun
     1  ayush
     1  benkbalog
     1  Csongor Kiss
     1  Ciara O'Brien
     1  Carlos Quiroz
     1  brunnerant
     1  =
     1  costa100
     1  Bunyod
     1  dieutth
     1  AlexSikia
     1  Brian Wignall
     1  张志豪
     1  felher
     1  Brandon Elam Barker
     1  fschueler
     1  gan74
     1  gnp
     1  gosubpl
     1  Bojan Dunaj
     1  iroha168
     1  Ben Hutchison
     1  Albert Serrallé Ríos
     1  Batanick
     1  Bartosz Krasiński
     1  August Nagro
     1  AngAng
     1  Adam Trousdale
     1  lpwisniewski
     1  manojo
     1  mentegy
     1  mikhail
     1  Mathias
     1  msosnicki
     1  Ang9876
     1  Max Ovsiankin
     1  Markus Kahl
     1  Markus Hauck
     1  Marc Karassev
     1  Mads Hartmann
     1  Lukas Ciszewski
     1  Ang Hao Yang
     1  Mike Samuel
     1  Lucas Jenß
     1  Li Haoyi
     1  Lanny Ripple
     1  Mohuety Kirisame
     1  Krzysiek Bochenek
     1  phderome
     1  Kevin Dreßler
     1  Keith Pinson
     1  Kazuyoshi Kato
     1  Kazuhiro Sera
     1  Niklas Vest
     1  Amadou CISSE
     1  riiswa
     1  Katrix
     1  Karol Chmist
     1  Ondra Pelech
```

[Scastie]: https://scastie.scala-lang.org/?target=dotty

[@odersky]: https://github.com/odersky
[@DarkDimius]: https://github.com/DarkDimius
[@smarter]: https://github.com/smarter
[@felixmulder]: https://github.com/felixmulder
[@nicolasstucki]: https://github.com/nicolasstucki
[@liufengyun]: https://github.com/liufengyun
[@OlivierBlanvillain]: https://github.com/OlivierBlanvillain
[@biboudis]: https://github.com/biboudis
[@allanrenucci]: https://github.com/allanrenucci
[@Blaisorblade]: https://github.com/Blaisorblade
[@Duhemm]: https://github.com/Duhemm
[@AleksanderBG]: https://github.com/AleksanderBG
[@milessabin]: https://github.com/milessabin
[@anatoliykmetyuk]: https://github.com/anatoliykmetyuk
