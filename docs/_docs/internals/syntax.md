---
layout: doc-page
title: "Scala 3 Syntax Summary"
---

The following description of Scala tokens uses literal characters `‘c’` when
referring to the ASCII fragment `\u0000` – `\u007F`.

_Unicode escapes_ are used to represent the [Unicode character](https://www.w3.org/International/articles/definitions-characters/) with the given
hexadecimal code:

```ebnf
UnicodeEscape ::= ‘\’ ‘u’ {‘u’} hexDigit hexDigit hexDigit hexDigit
hexDigit      ::= ‘0’ | … | ‘9’ | ‘A’ | … | ‘F’ | ‘a’ | … | ‘f’
```

Informal descriptions are typeset as `“some comment”`.

### Lexical Syntax

The lexical syntax of Scala is given by the following grammar in EBNF
form.

```ebnf
whiteSpace       ::=  ‘\u0020’ | ‘\u0009’ | ‘\u000D’ | ‘\u000A’
upper            ::=  ‘A’ | … | ‘Z’ | ‘\$’ | ‘_’  “… and Unicode category Lu”
lower            ::=  ‘a’ | … | ‘z’ “… and Unicode category Ll”
letter           ::=  upper | lower “… and Unicode categories Lo, Lt, Lm, Nl”
digit            ::=  ‘0’ | … | ‘9’
paren            ::=  ‘(’ | ‘)’ | ‘[’ | ‘]’ | ‘{’ | ‘}’
delim            ::=  ‘`’ | ‘'’ | ‘"’ | ‘.’ | ‘;’ | ‘,’
opchar           ::=  ‘!’ | ‘#’ | ‘%’ | ‘&’ | ‘*’ | ‘+’ | ‘-’ | ‘/’ | ‘:’ |
                      ‘<’ | ‘=’ | ‘>’ | ‘?’ | ‘@’ | ‘\’ | ‘^’ | ‘|’ | ‘~’
                      “… and Unicode categories Sm, So”
printableChar    ::=  “all characters in [\u0020, \u007E] inclusive”
charEscapeSeq    ::=  ‘\’ (‘b’ | ‘t’ | ‘n’ | ‘f’ | ‘r’ | ‘"’ | ‘'’ | ‘\’)

op               ::=  opchar {opchar}
varid            ::=  lower idrest
alphaid          ::=  upper idrest
                   |  varid
plainid          ::=  alphaid
                   |  op
id               ::=  plainid
                   |  ‘`’ { charNoBackQuoteOrNewline | UnicodeEscape | charEscapeSeq } ‘`’
idrest           ::=  {letter | digit} [‘_’ op]
quoteId          ::=  ‘'’ alphaid
spliceId         ::=  ‘$’ alphaid ;

integerLiteral   ::=  (decimalNumeral | hexNumeral) [‘L’ | ‘l’]
decimalNumeral   ::=  ‘0’ | nonZeroDigit [{digit | ‘_’} digit]
hexNumeral       ::=  ‘0’ (‘x’ | ‘X’) hexDigit [{hexDigit | ‘_’} hexDigit]
nonZeroDigit     ::=  ‘1’ | … | ‘9’

floatingPointLiteral
                 ::=  [decimalNumeral] ‘.’ digit [{digit | ‘_’} digit] [exponentPart] [floatType]
                   |  decimalNumeral exponentPart [floatType]
                   |  decimalNumeral floatType
exponentPart     ::=  (‘E’ | ‘e’) [‘+’ | ‘-’] digit [{digit | ‘_’} digit]
floatType        ::=  ‘F’ | ‘f’ | ‘D’ | ‘d’

booleanLiteral   ::=  ‘true’ | ‘false’

characterLiteral ::=  ‘'’ (printableChar | charEscapeSeq) ‘'’

stringLiteral    ::=  ‘"’ {stringElement} ‘"’
                   |  ‘"""’ multiLineChars ‘"""’
stringElement    ::=  printableChar \ (‘"’ | ‘\’)
                   |  UnicodeEscape
                   |  charEscapeSeq
multiLineChars   ::=  {[‘"’] [‘"’] char \ ‘"’} {‘"’}
processedStringLiteral
                 ::=  alphaid ‘"’ {[‘\’] processedStringPart | ‘\\’ | ‘\"’} ‘"’
                   |  alphaid ‘"""’ {[‘"’] [‘"’] char \ (‘"’ | ‘$’) | escape} {‘"’} ‘"""’
processedStringPart
                 ::= printableChar \ (‘"’ | ‘$’ | ‘\’) | escape
escape           ::=  ‘$$’
                   |  ‘$’ letter { letter | digit }
                   |  ‘{’ Block  [‘;’ whiteSpace stringFormat whiteSpace] ‘}’
stringFormat     ::=  {printableChar \ (‘"’ | ‘}’ | ‘ ’ | ‘\t’ | ‘\n’)}

symbolLiteral    ::=  ‘'’ plainid // until 2.13

comment          ::=  ‘/*’ “any sequence of characters; nested comments are allowed” ‘*/’
                   |  ‘//’ “any sequence of characters up to end of line”

nl               ::=  “new line character”
semi             ::=  ‘;’ |  nl {nl}
```


## Optional Braces

The lexical analyzer also inserts `indent` and `outdent` tokens that represent regions of indented code [at certain points](../reference/other-new-features/indentation.md)

In the context-free productions below we use the notation `<<< ts >>>`
to indicate a token sequence `ts` that is either enclosed in a pair of braces `{ ts }` or that constitutes an indented region `indent ts outdent`. Analogously, the
notation `:<<< ts >>>` indicates a token sequence `ts` that is either enclosed in a pair of braces `{ ts }` or that constitutes an indented region `indent ts outdent` that follows
a `:` at the end of a line.


```
 <<< ts >>>   ::=  ‘{’ ts ‘}’
                |  indent ts outdent
:<<< ts >>>   ::=  [nl] ‘{’ ts ‘}’
                |  `:` indent ts outdent
```

## Keywords

### Regular keywords

```
abstract  case      catch     class     def       do        else
enum      export    extends   false     final     finally   for
given     if        implicit  import    lazy      match     new
null      object    override  package   private   protected return
sealed    super     then      throw     trait     true      try
type      val       var       while     with      yield
:         =         <-        =>        <:        >:        #
@         =>>       ?=>
```

### Soft keywords

```
as  derives  end  extension  infix  inline  opaque  open  transparent  using  |  *  +  -
```

See the [separate section on soft keywords](../reference/soft-modifier.md) for additional
details on where a soft keyword is recognized.

## Context-free Syntax

The context-free syntax of Scala is given by the following EBNF
grammar:

### Literals and Paths
```ebnf
SimpleLiteral     ::=  [‘-’] integerLiteral
                    |  [‘-’] floatingPointLiteral
                    |  booleanLiteral
                    |  characterLiteral
                    |  stringLiteral
Literal           ::=  SimpleLiteral
                    |  processedStringLiteral
                    |  symbolLiteral
                    |  ‘null’

QualId            ::=  id {‘.’ id}
ids               ::=  id {‘,’ id}

SimpleRef         ::=  id
                    |  [id ‘.’] ‘this’
                    |  [id ‘.’] ‘super’ [ClassQualifier] ‘.’ id

ClassQualifier    ::=  ‘[’ id ‘]’
```

### Types
```ebnf
Type              ::=  FunType
                    |  HkTypeParamClause ‘=>>’ Type                             LambdaTypeTree(ps, t)
                    |  FunParamClause ‘=>>’ Type                                TermLambdaTypeTree(ps, t)
                    |  MatchType
                    |  InfixType
FunType           ::=  FunTypeArgs (‘=>’ | ‘?=>’) Type                          Function(ts, t)
                    |  HKTypeParamClause '=>' Type                              PolyFunction(ps, t)
FunTypeArgs       ::=  InfixType
                    |  ‘(’ [ FunArgTypes ] ‘)’
                    |  FunParamClause
FunParamClause    ::=  ‘(’ TypedFunParam {‘,’ TypedFunParam } ‘)’
TypedFunParam     ::=  id ‘:’ Type
MatchType         ::=  InfixType `match` <<< TypeCaseClauses >>>
InfixType         ::=  RefinedType {id [nl] RefinedType}                        InfixOp(t1, op, t2)
RefinedType       ::=  AnnotType {[nl] Refinement}                              RefinedTypeTree(t, ds)
AnnotType         ::=  SimpleType {Annotation}                                  Annotated(t, annot)

SimpleType        ::=  SimpleLiteral                                            SingletonTypeTree(l)
                    |  ‘?’ TypeBounds
                    |  SimpleType1
SimpleType1       ::=  id                                                       Ident(name)
                    |  Singleton ‘.’ id                                         Select(t, name)
                    |  Singleton ‘.’ ‘type’                                     SingletonTypeTree(p)
                    |  ‘(’ Types ‘)’                                            Tuple(ts)
                    |  Refinement                                               RefinedTypeTree(EmptyTree, refinement)
                    |  TypeSplice                                               -- deprecated syntax
                    |  SimpleType1 TypeArgs                                     AppliedTypeTree(t, args)
                    |  SimpleType1 ‘#’ id                                       Select(t, name)
Singleton         ::=  SimpleRef
                    |  SimpleLiteral
                    |  Singleton ‘.’ id
Singletons        ::=  Singleton { ‘,’ Singleton }
FunArgType        ::=  Type
                    |  ‘=>’ Type                                                PrefixOp(=>, t)
FunArgTypes       ::=  FunArgType { ‘,’ FunArgType }
ParamType         ::=  [‘=>’] ParamValueType
ParamValueType    ::=  Type [‘*’]                                               PostfixOp(t, "*")
TypeArgs          ::=  ‘[’ Types ‘]’                                            ts
Refinement        ::=  ‘{’ [RefineDcl] {semi [RefineDcl]} ‘}’                   ds
TypeBounds        ::=  [‘>:’ Type] [‘<:’ Type]                                  TypeBoundsTree(lo, hi)
TypeParamBounds   ::=  TypeBounds {‘:’ Type}                                    ContextBounds(typeBounds, tps)
Types             ::=  Type {‘,’ Type}
```

### Expressions
```ebnf
Expr              ::=  FunParams (‘=>’ | ‘?=>’) Expr                            Function(args, expr), Function(ValDef([implicit], id, TypeTree(), EmptyTree), expr)
                    |  HkTypeParamClause ‘=>’ Expr                              PolyFunction(ts, expr)
                    |  Expr1
BlockResult       ::=  FunParams (‘=>’ | ‘?=>’) Block
                    |  HkTypeParamClause ‘=>’ Block
                    |  Expr1
FunParams         ::=  Bindings
                    |  id
                    |  ‘_’
Expr1             ::=  [‘inline’] ‘if’ ‘(’ Expr ‘)’ {nl} Expr [[semi] ‘else’ Expr] If(Parens(cond), thenp, elsep?)
                    |  [‘inline’] ‘if’  Expr ‘then’ Expr [[semi] ‘else’ Expr]    If(cond, thenp, elsep?)
                    |  ‘while’ ‘(’ Expr ‘)’ {nl} Expr                           WhileDo(Parens(cond), body)
                    |  ‘while’ Expr ‘do’ Expr                                   WhileDo(cond, body)
                    |  ‘try’ Expr Catches [‘finally’ Expr]                      Try(expr, catches, expr?)
                    |  ‘try’ Expr [‘finally’ Expr]                              Try(expr, Nil, expr?)
                    |  ‘throw’ Expr                                             Throw(expr)
                    |  ‘return’ [Expr]                                          Return(expr?)
                    |  ForExpr
                    |  [SimpleExpr ‘.’] id ‘=’ Expr                             Assign(expr, expr)
                    |  PrefixOperator SimpleExpr ‘=’ Expr                       Assign(expr, expr)
                    |  SimpleExpr ArgumentExprs ‘=’ Expr                        Assign(expr, expr)
                    |  PostfixExpr [Ascription]
                    |  ‘inline’ InfixExpr MatchClause
Ascription        ::=  ‘:’ InfixType                                            Typed(expr, tp)
                    |  ‘:’ Annotation {Annotation}                              Typed(expr, Annotated(EmptyTree, annot)*)
Catches           ::=  ‘catch’ (Expr | ExprCaseClause)
PostfixExpr       ::=  InfixExpr [id]                                           PostfixOp(expr, op)
InfixExpr         ::=  PrefixExpr
                    |  InfixExpr id [nl] InfixExpr                              InfixOp(expr, op, expr)
                    |  InfixExpr id ‘:’ IndentedExpr
                    |  InfixExpr MatchClause
MatchClause       ::=  ‘match’ <<< CaseClauses >>>                              Match(expr, cases)
PrefixExpr        ::=  [PrefixOperator] SimpleExpr                             PrefixOp(expr, op)
PrefixOperator    ::=  ‘-’ | ‘+’ | ‘~’ | ‘!’
SimpleExpr        ::=  SimpleRef
                    |  Literal
                    |  ‘_’
                    |  BlockExpr
                    |  ExprSplice
                    |  Quoted
                    |  quoteId                                                  -- only inside splices
                    |  ‘new’ ConstrApp {‘with’ ConstrApp} [TemplateBody]        New(constr | templ)
                    |  ‘new’ TemplateBody
                    |  ‘(’ ExprsInParens ‘)’                                    Parens(exprs)
                    |  SimpleExpr ‘.’ id                                        Select(expr, id)
                    |  SimpleExpr ‘.’ MatchClause
                    |  SimpleExpr TypeArgs                                      TypeApply(expr, args)
                    |  SimpleExpr ArgumentExprs                                 Apply(expr, args)
                    |  SimpleExpr ‘:’ IndentedExpr                              -- under language.experimental.fewerBraces
                    |  SimpleExpr FunParams (‘=>’ | ‘?=>’) IndentedExpr         -- under language.experimental.fewerBraces
                    |  SimpleExpr ‘_’                                           PostfixOp(expr, _) (to be dropped)
                    |  XmlExpr													                        -- to be dropped
IndentedExpr      ::=  indent CaseClauses | Block outdent
Quoted            ::=  ‘'’ ‘{’ Block ‘}’
                    |  ‘'’ ‘[’ Type ‘]’
ExprSplice        ::= spliceId                                                  -- if inside quoted block
                    |  ‘$’ ‘{’ Block ‘}’                                        -- unless inside quoted pattern
                    |  ‘$’ ‘{’ Pattern ‘}’                                      -- when inside quoted pattern
TypeSplice        ::= spliceId                                                  -- if inside quoted type -- deprecated syntax
                    |  ‘$’ ‘{’ Block ‘}’                                        -- unless inside quoted type pattern -- deprecated syntax
                    |  ‘$’ ‘{’ Pattern ‘}’                                      -- when inside quoted type pattern -- deprecated syntax
ExprsInParens     ::=  ExprInParens {‘,’ ExprInParens}
ExprInParens      ::=  PostfixExpr ‘:’ Type                                     -- normal Expr allows only RefinedType here
                    |  Expr
ParArgumentExprs  ::=  ‘(’ [‘using’] ExprsInParens ‘)’                          exprs
                    |  ‘(’ [ExprsInParens ‘,’] PostfixExpr ‘*’ ‘)’              exprs :+ Typed(expr, Ident(wildcardStar))
ArgumentExprs     ::=  ParArgumentExprs
                    |  BlockExpr
BlockExpr         ::=  <<< CaseClauses | Block >>>
Block             ::=  {BlockStat semi} [BlockResult]                           Block(stats, expr?)
BlockStat         ::=  Import
                    |  {Annotation {nl}} {LocalModifier} Def
                    |  Extension
                    |  Expr1
                    |  EndMarker

ForExpr           ::=  ‘for’ ‘(’ Enumerators0 ‘)’ {nl} [‘do‘ | ‘yield’] Expr     ForYield(enums, expr) / ForDo(enums, expr)
                    |  ‘for’ ‘{’ Enumerators0 ‘}’ {nl} [‘do‘ | ‘yield’] Expr
                    |  ‘for’     Enumerators0          (‘do‘ | ‘yield’) Expr
Enumerators0      ::=  {nl} Enumerators [semi]
Enumerators       ::=  Generator {semi Enumerator | Guard}
Enumerator        ::=  Generator
                    |  Guard {Guard}
                    |  Pattern1 ‘=’ Expr                                        GenAlias(pat, expr)
Generator         ::=  [‘case’] Pattern1 ‘<-’ Expr                              GenFrom(pat, expr)
Guard             ::=  ‘if’ PostfixExpr

CaseClauses       ::=  CaseClause { CaseClause }                                Match(EmptyTree, cases)
CaseClause        ::=  ‘case’ Pattern [Guard] ‘=>’ Block                        CaseDef(pat, guard?, block)   // block starts at =>
ExprCaseClause    ::=  ‘case’ Pattern [Guard] ‘=>’ Expr
TypeCaseClauses   ::=  TypeCaseClause { TypeCaseClause }
TypeCaseClause    ::=  ‘case’ (InfixType | ‘_’) ‘=>’ Type [semi]

Pattern           ::=  Pattern1 { ‘|’ Pattern1 }                                Alternative(pats)
Pattern1          ::=  Pattern2 [‘:’ RefinedType]                               Bind(name, Typed(Ident(wildcard), tpe))
Pattern2          ::=  [id ‘@’] InfixPattern                                    Bind(name, pat)
InfixPattern      ::=  SimplePattern { id [nl] SimplePattern }                  InfixOp(pat, op, pat)
SimplePattern     ::=  PatVar                                                   Ident(wildcard)
                    |  Literal                                                  Bind(name, Ident(wildcard))
                    |  ‘(’ [Patterns] ‘)’                                       Parens(pats) Tuple(pats)
                    |  Quoted
                    |  XmlPattern												(to be dropped)
                    |  SimplePattern1 [TypeArgs] [ArgumentPatterns]
                    |  ‘given’ RefinedType
SimplePattern1    ::=  SimpleRef
                    |  SimplePattern1 ‘.’ id
PatVar            ::=  varid
                    |  ‘_’
Patterns          ::=  Pattern {‘,’ Pattern}
ArgumentPatterns  ::=  ‘(’ [Patterns] ‘)’                                       Apply(fn, pats)
                    |  ‘(’ [Patterns ‘,’] PatVar ‘*’ ‘)’
```

### Type and Value Parameters
```ebnf
ClsTypeParamClause::=  ‘[’ ClsTypeParam {‘,’ ClsTypeParam} ‘]’
ClsTypeParam      ::=  {Annotation} [‘+’ | ‘-’]                                 TypeDef(Modifiers, name, tparams, bounds)
                       id [HkTypeParamClause] TypeParamBounds                   Bound(below, above, context)

DefTypeParamClause::=  ‘[’ DefTypeParam {‘,’ DefTypeParam} ‘]’
DefTypeParam      ::=  {Annotation} id [HkTypeParamClause] TypeParamBounds

TypTypeParamClause::=  ‘[’ TypTypeParam {‘,’ TypTypeParam} ‘]’
TypTypeParam      ::=  {Annotation} id [HkTypeParamClause] TypeBounds

HkTypeParamClause ::=  ‘[’ HkTypeParam {‘,’ HkTypeParam} ‘]’
HkTypeParam       ::=  {Annotation} [‘+’ | ‘-’] (id [HkTypeParamClause] | ‘_’)
                       TypeBounds

ClsParamClauses   ::=  {ClsParamClause} [[nl] ‘(’ [‘implicit’] ClsParams ‘)’]
ClsParamClause    ::=  [nl] ‘(’ ClsParams ‘)’
                    |  [nl] ‘(’ ‘using’ (ClsParams | FunArgTypes) ‘)’
ClsParams         ::=  ClsParam {‘,’ ClsParam}
ClsParam          ::=  {Annotation}                                             ValDef(mods, id, tpe, expr) -- point of mods on val/var
                       [{Modifier} (‘val’ | ‘var’) | ‘inline’] Param
Param             ::=  id ‘:’ ParamType [‘=’ Expr]

DefParamClauses   ::=  {DefParamClause} [[nl] ‘(’ [‘implicit’] DefParams ‘)’]
DefParamClause    ::=  [nl] ‘(’ DefParams ‘)’ | UsingParamClause
UsingParamClause  ::=  [nl] ‘(’ ‘using’ (DefParams | FunArgTypes) ‘)’
DefParams         ::=  DefParam {‘,’ DefParam}
DefParam          ::=  {Annotation} [‘inline’] Param                            ValDef(mods, id, tpe, expr) -- point of mods at id.
```

### Bindings and Imports
```ebnf
Bindings          ::=  ‘(’ [Binding {‘,’ Binding}] ‘)’
Binding           ::=  (id | ‘_’) [‘:’ Type]                                    ValDef(_, id, tpe, EmptyTree)

Modifier          ::=  LocalModifier
                    |  AccessModifier
                    |  ‘override’
                    |  ‘opaque’
LocalModifier     ::=  ‘abstract’
                    |  ‘final’
                    |  ‘sealed’
                    |  ‘open’
                    |  ‘implicit’
                    |  ‘lazy’
                    |  ‘inline’
AccessModifier    ::=  (‘private’ | ‘protected’) [AccessQualifier]
AccessQualifier   ::=  ‘[’ id ‘]’

Annotation        ::=  ‘@’ SimpleType1 {ParArgumentExprs}                         Apply(tpe, args)

Import            ::=  ‘import’ ImportExpr {‘,’ ImportExpr}
Export            ::=  ‘export’ ImportExpr {‘,’ ImportExpr}
ImportExpr        ::=  SimpleRef {‘.’ id} ‘.’ ImportSpec                          Import(expr, sels)
                    |  SimpleRef ‘as’ id                                          Import(EmptyTree, ImportSelector(ref, id))
ImportSpec        ::=  NamedSelector
                    |  WildcardSelector
                    | ‘{’ ImportSelectors) ‘}’
NamedSelector     ::=  id [‘as’ (id | ‘_’)]
WildCardSelector  ::=  ‘*' | ‘given’ [InfixType]
ImportSelectors   ::=  NamedSelector [‘,’ ImportSelectors]
                    |  WildCardSelector {‘,’ WildCardSelector}

EndMarker         ::=  ‘end’ EndMarkerTag    -- when followed by EOL
EndMarkerTag      ::=  id | ‘if’ | ‘while’ | ‘for’ | ‘match’ | ‘try’
                    |  ‘new’ | ‘this’ | ‘given’ | ‘extension’ | ‘val’
```

### Declarations and Definitions
```ebnf
RefineDcl         ::=  ‘val’ ValDcl
                    |  ‘def’ DefDcl
                    |  ‘type’ {nl} TypeDcl
Dcl               ::=  RefineDcl
                    |  ‘var’ VarDcl
ValDcl            ::=  ids ‘:’ Type                                             PatDef(_, ids, tpe, EmptyTree)
VarDcl            ::=  ids ‘:’ Type                                             PatDef(_, ids, tpe, EmptyTree)
DefDcl            ::=  DefSig ‘:’ Type                                          DefDef(_, name, tparams, vparamss, tpe, EmptyTree)
DefSig            ::=  id [DefTypeParamClause] DefParamClauses
TypeDcl           ::=  id [TypeParamClause] {FunParamClause} TypeBounds         TypeDefTree(_, name, tparams, bound
                       [‘=’ Type]

Def               ::=  ‘val’ PatDef
                    |  ‘var’ PatDef
                    |  ‘def’ DefDef
                    |  ‘type’ {nl} TypeDcl
                    |  TmplDef
PatDef            ::=  ids [‘:’ Type] ‘=’ Expr
                    |  Pattern2 [‘:’ Type] ‘=’ Expr                             PatDef(_, pats, tpe?, expr)
DefDef            ::=  DefSig [‘:’ Type] ‘=’ Expr                               DefDef(_, name, tparams, vparamss, tpe, expr)
                    |  ‘this’ DefParamClause DefParamClauses ‘=’ ConstrExpr     DefDef(_, <init>, Nil, vparamss, EmptyTree, expr | Block)

TmplDef           ::=  ([‘case’] ‘class’ | ‘trait’) ClassDef
                    |  [‘case’] ‘object’ ObjectDef
                    |  ‘enum’ EnumDef
                    |  ‘given’ GivenDef
ClassDef          ::=  id ClassConstr [Template]                                ClassDef(mods, name, tparams, templ)
ClassConstr       ::=  [ClsTypeParamClause] [ConstrMods] ClsParamClauses        with DefDef(_, <init>, Nil, vparamss, EmptyTree, EmptyTree) as first stat
ConstrMods        ::=  {Annotation} [AccessModifier]
ObjectDef         ::=  id [Template]                                            ModuleDef(mods, name, template)  // no constructor
EnumDef           ::=  id ClassConstr InheritClauses EnumBody
GivenDef          ::=  [GivenSig] (AnnotType [‘=’ Expr] | StructuralInstance)
GivenSig          ::=  [id] [DefTypeParamClause] {UsingParamClause} ‘:’         -- one of `id`, `DefParamClause`, `UsingParamClause` must be present
StructuralInstance ::=  ConstrApp {‘with’ ConstrApp} [‘with’ TemplateBody]
Extension         ::=  ‘extension’ [DefTypeParamClause] {UsingParamClause}
                       ‘(’ DefParam ‘)’ {UsingParamClause} ExtMethods
ExtMethods        ::=  ExtMethod | [nl] <<< ExtMethod {semi ExtMethod} >>>
ExtMethod         ::=  {Annotation [nl]} {Modifier} ‘def’ DefDef
                    |  Export
Template          ::=  InheritClauses [TemplateBody]
InheritClauses    ::=  [‘extends’ ConstrApps] [‘derives’ QualId {‘,’ QualId}]
ConstrApps        ::=  ConstrApp ({‘,’ ConstrApp} | {‘with’ ConstrApp})
ConstrApp         ::=  SimpleType1 {Annotation} {ParArgumentExprs}
ConstrExpr        ::=  SelfInvocation
                    |  <<< SelfInvocation {semi BlockStat} >>>
SelfInvocation    ::=  ‘this’ ArgumentExprs {ArgumentExprs}

TemplateBody      ::=  :<<< [SelfType] TemplateStat {semi TemplateStat} >>>
TemplateStat      ::=  Import
                    |  Export
                    |  {Annotation [nl]} {Modifier} Def
                    |  {Annotation [nl]} {Modifier} Dcl
                    |  Extension
                    |  Expr1
                    |  EndMarker
                    |
SelfType          ::=  id [‘:’ InfixType] ‘=>’                                  ValDef(_, name, tpt, _)
                    |  ‘this’ ‘:’ InfixType ‘=>’

EnumBody          ::=  :<<< [SelfType] EnumStat {semi EnumStat} >>>
EnumStat          ::=  TemplateStat
                    |  {Annotation [nl]} {Modifier} EnumCase
EnumCase          ::=  ‘case’ (id ClassConstr [‘extends’ ConstrApps]] | ids)

TopStats          ::=  TopStat {semi TopStat}
TopStat           ::=  Import
                    |  Export
                    |  {Annotation [nl]} {Modifier} Def
                    |  Extension
                    |  Packaging
                    |  PackageObject
                    |  EndMarker
                    |
Packaging         ::=  ‘package’ QualId :<<< TopStats >>>
PackageObject     ::=  ‘package’ ‘object’ ObjectDef

CompilationUnit   ::=  {‘package’ QualId semi} TopStats
```
