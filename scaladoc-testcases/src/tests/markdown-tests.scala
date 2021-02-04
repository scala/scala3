package tests

/**
  * * a
  * * b
  * * c
  *
  * - a
  * - b
  * - c
  *
  * 1. a
  * 1. b
  * 1. c
  *
  *
  * If the following list was indented one space less, it wouldn't parse
  * properly. That is, the first nested list would not be nested. This is
  * because ATTW we trim _up to two_ spaces between the star "gutter" and actual
  * comment bodies.
  *
  *  * a
  *    - a.a
  *    - a.b
  *    - a.c
  *  * b
  *    1. b.1
  *    2. b.2
  *    3. b.3
  *       * b.3.a
  *       * b.3.b
  *       * b.3.c
  */
class MdLists

/**
  * | day         | time  |   spent |
  * |:------------|:-----:|--------:|
  * | nov. 2. tue | 10:00 |  4h 40m |
  * | nov. 3. thu | 11:00 |      4h |
  * | nov. 7. mon | 10:20 |  4h 20m |
  * | total:             ||     13h |
  */
class MdTables
