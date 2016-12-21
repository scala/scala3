---
layout: default
title: "Scala Syntax Summary"
---

The following descriptions of Scala tokens uses literal characters `‘c’` when
referring to the ASCII fragment `\u0000` – `\u007F`.

_Unicode escapes_ are used to represent the Unicode character with the given
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
upper            ::=  ‘A’ | $\cdots$ | ‘Z’ | ‘$\Dollar$’ | ‘_’ $\mbox{\rm\em and Unicode category Lu}$
lower            ::=  ‘a’ | $\cdots$ | ‘z’ $\mbox{\rm\em and Unicode category Ll}$
letter           ::=  upper | lower $\mbox{\rm\em and Unicode categories Lo, Lt, Nl}$
digit            ::=  ‘0’ | $\cdots$ | ‘9’
opchar           ::=  $\mbox{\rm\em ``all other characters in \U{0020-007F} and Unicode}$
                      $\mbox{\rm\em categories Sm, So except parentheses ([{}]) and periods''}$

op               ::=  opchar {opchar}
varid            ::=  lower idrest
alphaid          ::=  upper idrest
                   |  varid
plainid          ::=  alphaid
                   |  op
id               ::=  plainid
                   |  ‘\`’ stringLit ‘\`’
                   |  INT                           // interpolation id, only for quasi-quotes
idrest           ::=  {letter | digit} [‘_’ op]

integerLiteral   ::=  (decimalNumeral | hexNumera) [‘L’ | ‘l’]
decimalNumeral   ::=  ‘0’ | nonZeroDigit {digit}
hexNumeral       ::=  ‘0’ ‘x’ hexDigit {hexDigit}
digit            ::=  ‘0’ | nonZeroDigit
nonZeroDigit     ::=  ‘1’ | $\cdots$ | ‘9’
octalDigit       ::=  ‘0’ | $\cdots$ | ‘7’

floatingPointLiteral
                 ::=  digit {digit} ‘.’ {digit} [exponentPart] [floatType]
                   |  ‘.’ digit {digit} [exponentPart] [floatType]
                   |  digit {digit} exponentPart [floatType]
                   |  digit {digit} [exponentPart] floatType

exponentPart     ::=  (‘E’ | ‘e’) [‘+’ | ‘-’] digit {digit}
floatType        ::=  ‘F’ | ‘f’ | ‘D’ | ‘d’
booleanLiteral   ::=  ‘true’ | ‘false’

characterLiteral ::=  ‘\'’ printableChar ‘\'’
                   |  ‘\'’ charEscapeSeq ‘\'’

stringLiteral    ::=  ‘"’ {stringElement} ‘"’
                   |  ‘"""’ {[‘"’] [‘"’] char \ ‘"’} {‘"’} ‘"""’
stringElement    ::=  printableChar \ (‘"’ | ‘\’)
                   |  charEscapeSeq
charEscapeSeq    ::=  ‘\b’ | ‘\n’ | ‘\t’ | ‘\f’ | ‘\r’ | ‘"’ | ‘'’ | ‘\\’

processedStringLiteral
                 ::=  alphaid ‘"’ {printableChar \ (‘"’ | ‘$’) | escape} ‘"’
                   |  alphaid ‘"""’ {[‘"’] [‘"’] char \ (‘"’ | ‘$’) | escape} {‘"’} ‘"""’
escape           ::=  ‘$$’ \comment{$}
                   |  ‘$’ letter { letter | digit }
                   |  ‘{’ Block  [‘;’ whiteSpace stringFormat whiteSpace] ‘}’
stringFormat     ::=  {printableChar \ (‘"’ | ‘}’ | ‘ ’ | ‘\t’ | ‘\n’)}
whiteSpace       ::=  {‘ ’ | ‘\t’}

symbolLiteral    ::=  ‘'’ plainid

comment          ::=  ‘/*’ $\mbox{\rm\em ``any sequence of characters''}$ ‘*/’
                   |  ‘//’ $\mbox{\rm\em ``any sequence of characters up to end of line''}$

nl               ::=  $\mbox{\rm\em ``new line character''}$
semi             ::=  ‘;’ |  nl {nl}
```

## Context-free Syntax

The context-free syntax of Scala is given by the following EBNF
grammar:

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

Path              ::=  StableId
                    |  [id ‘.’] ‘this’
StableId          ::=  id
                    |  Path ‘.’ id
                    |  [id ‘.’] ‘super’ [ClassQualifier] ‘.’ id
ClassQualifier    ::=  ‘[’ id ‘]’

Type              ::=  [‘implicit’] FunArgTypes ‘=>’ Type
                    |  HkTypeParamClause ‘=>’ Type
                    |  InfixType
FunArgTypes       ::=  InfixType
                    | ‘(’ [ FunArgType {‘,’ FunArgType } ] ‘)’
InfixType         ::=  RefinedType {id [nl] RefinedType}
RefinedType       ::=  WithType {[nl] Refinement}
WithType          ::=  AnnotType {‘with’ AnnotType}
AnnotType         ::=  SimpleType {Annotation}
SimpleType        ::=  SimpleType (TypeArgs | NamedTypeArgs)
                    |  SimpleType ‘#’ id
                    |  StableId
                    |  Path ‘.’ ‘type’
                    |  ‘(’ ArgTypes ‘)’
                    |  ‘_’ TypeBounds
                    |  Refinement
                    |  SimpleLiteral
ArgTypes          ::=  Type {‘,’ Type}
                    |  NamedTypeArg {‘,’ NamedTypeArg }
FunArgType        ::=  Type
                    |  ‘=>’ Type
ParamType         ::=  [‘=>’] ParamValueType
ParamValueType    ::=  Type [‘*’]
TypeArgs          ::=  ‘[’ ArgTypes ‘]’
NamedTypeArg      ::=  id ‘=’ Type
NamedTypeArgs     ::=  ‘[’ NamedTypeArg {‘,’ NamedTypeArg} ‘]’
Refinement        ::=  ‘{’ [Dcl] {semi [Dcl]} ‘}’
TypeBounds        ::=  [‘>:’ Type] [`<: Type] | INT
TypeParamBounds   ::=  TypeBounds {‘<%’ Type} {‘:’ Type}

Expr              ::=  [‘implicit’] FunParams ‘=>’ Expr
FunParams         ::=  Bindings
                    |  id
                    |  ‘_’
ExprInParens      ::=  PostfixExpr ‘:’ Type
                    |  Expr
BlockResult       ::=   [‘implicit’] FunParams ‘=>’ Block
                    |  Expr1
Expr1             ::=  ‘if’ ‘(’ Expr ‘)’ {nl} Expr [[semi] else Expr]
                    |  ‘if’ Expr ‘then’ Expr [[semi] else Expr]
                    |  ‘while’ ‘(’ Expr ‘)’ {nl} Expr
                    |  ‘while’ Expr ‘do’ Expr
                    |  ‘do’ Expr [semi] ‘while’ Expr
                    |  ‘try’ Expr Catches [‘finally’ Expr]
                    |  ‘try’ Expr [‘finally’ Expr]
                    |  ‘throw’ Expr
                    |  ‘return’ [Expr]
                    |  ForExpr
                    |  [SimpleExpr ‘.’] id ‘=’ Expr
                    |  SimpleExpr1 ArgumentExprs ‘=’ Expr
                    |  PostfixExpr [Ascription]
                    |  PostfixExpr ‘match’ ‘{’ CaseClauses ‘}’
Ascription        ::=  ‘:’ InfixType
                    |  ‘:’ Annotation {Annotation}
Catches           ::=  ‘catch’ Expr
PostfixExpr       ::=  InfixExpr [id]
InfixExpr         ::=  PrefixExpr
                    |  InfixExpr id [nl] InfixExpr
PrefixExpr        ::=  [‘-’ | ‘+’ | ‘~’ | ‘!’] SimpleExpr
SimpleExpr        ::=  ‘new’ Template
                    |  BlockExpr
                    |  SimpleExpr1 [‘_’]
SimpleExpr1       ::=  Literal
                    |  Path
                    |  ‘_’
                    |  ‘(’ ExprsInParens2 ‘)’
                    |  SimpleExpr ‘.’ id
                    |  SimpleExpr (TypeArgs | NamedTypeArgs)
                    |  SimpleExpr1 ArgumentExprs
                    |  XmlExpr
ExprsInParens     ::=  ExprInParens {‘,’ ExprInParens}
ParArgumentExprs  ::=  ‘(’ [ExprsInParens] ‘)’
                    |  ‘(’ [ExprsInParens ‘,’] PostfixExpr ‘:’ ‘_’ ‘*’ ‘)’
ArgumentExprs     ::=  ParArgumentExprs
                    |  [nl] BlockExpr
BlockExpr         ::=  ‘{’ CaseClauses ‘}’
                    |  ‘{’ Block ‘}’
Block             ::=  {BlockStat semi} [BlockResult]
BlockStat         ::=  Import
                    |  {Annotation} [‘implicit’ | ‘lazy’] Def
                    |  {Annotation} {LocalModifier} TmplDef
                    |  Expr1
                    |

ForExpr           ::=  ‘for’ (‘(’ Enumerators ‘)’ | ‘{’ Enumerators ‘}’)
                       {nl} [‘yield’] Expr
                    |  ‘for’ Enumerators (‘do’ Expr | ‘yield’ Expr)

Enumerators       ::=  Generator {semi Enumerator | Guard}
Enumerator        ::=  Generator
                    |  Guard
                    |  Pattern1 ‘=’ Expr
Generator         ::=  Pattern1 ‘<-’ Expr
Guard             ::=  ‘if’ PostfixExpr

CaseClauses       ::=  CaseClause { CaseClause }
CaseClause        ::=  ‘case’ (Pattern [Guard] ‘=>’ Block | INT)

Pattern           ::=  Pattern1 { ‘|’ Pattern1 }
Pattern1          ::=  PatVar ‘:’ RefinedType
                    |  Pattern2
Pattern2          ::=  [varid ‘@’] InfixPattern
InfixPattern      ::=  SimplePattern { id [nl] SimplePattern }
SimplePattern     ::=  PatVar
                    |  Literal
                    |  ‘(’ [Patterns] ‘)’
                    |  XmlPattern
                    |  SimplePattern1 [TypeArgs] [ArgumentPatterns]
SimplePattern1    ::=  Path
                    |  ‘{’ Block ‘}’
                    |  SimplePattern1 ‘.’ id
PatVar            ::=  varid
                    |  ‘_’
Patterns          ::=  Pattern {‘,’ Pattern}
ArgumentPatterns  ::=  ‘(’ [Patterns] ‘)’
                    |  ‘(’ [Patterns ‘,’] Pattern2 ‘:’ ‘_’ ‘*’ ')

ClsTypeParamClause::=  ‘[’ ClsTypeParam {‘,’ ClsTypeParam} ‘]’
ClsTypeParam      ::=  {Annotation} [{Modifier} type] [‘+’ | ‘-’]
                       id [HkTypeParamClause] TypeParamBounds

DefTypeParamClause::=  ‘[’ DefTypeParam {‘,’ DefTypeParam} ‘]’
DefTypeParam      ::=  {Annotation} id [HkTypeParamClause] TypeParamBounds

TypTypeParamCaluse::=  ‘[’ TypTypeParam {‘,’ TypTypeParam} ‘]’
TypTypeParam      ::=  {Annotation} id [HkTypeParamClause] TypeBounds

HkTypeParamClause ::=  ‘[’ HkTypeParam {‘,’ HkTypeParam} ‘]’
HkTypeParam       ::=  {Annotation} [‘+’ | ‘-’] (Id[HkTypeParamClause] | ‘_’)
                       TypeBounds

ClsParamClauses   ::=  {ClsParamClause} [[nl] ‘(’ ‘implicit’ ClsParams ‘)’]
ClsParamClause    ::=  [nl] ‘(’ [ClsParams] ‘)’
ClsParams         ::=  ClsParam {‘’ ClsParam}
ClsParam          ::=  {Annotation}
                       [{Modifier} (‘val’ | ‘var’) | ‘inline’] Param
Param             ::=  id ‘:’ ParamType [‘=’ Expr]
                    |  INT

DefParamClauses   ::=  {DefParamClause} [[nl] ‘(’ ‘implicit’ DefParams ‘)’]
DefParamClause    ::=  [nl] ‘(’ [DefParams] ‘)’
DefParams         ::=  DefParam {‘,’ DefParam}
DefParam          ::=  {Annotation} [‘inline’] Param

Bindings          ::=  ‘(’ Binding {‘,’ Binding}] ‘)’
Binding           ::=  (id | ‘_’) [‘:’ Type]

Modifier          ::=  LocalModifier
                    |  AccessModifier
                    |  ‘override’
LocalModifier     ::=  ‘abstract’
                    |  ‘final’
                    |  ‘sealed’
                    |  ‘implicit’
                    |  ‘lazy’
AccessModifier    ::=  (‘private’ | ‘protected’) [AccessQualifier]
AccessQualifier   ::=  ‘[’ (id | ‘this’) ‘]’

Annotation        ::=  ‘@’ SimpleType {ParArgumentExprs}

TemplateBody      ::=  [nl] ‘{’ [SelfType] TemplateStat {semi TemplateStat} `}
TemplateStat      ::=  Import
                    |  {Annotation [nl]} {Modifier} Def
                    |  {Annotation [nl]} {Modifier} Dcl
                    |  Expr1
                    |
SelfType          ::=  id [‘:’ InfixType] ‘=>’
                    |  ‘this’ ‘:’ InfixType `=>

Import            ::=  ‘import’ ImportExpr {‘,’ ImportExpr}
ImportExpr        ::=  StableId ‘.’ (id | ‘_’ | ImportSelectors)
ImportSelectors   ::=  ‘{’ {ImportSelector ‘,’} (ImportSelector | ‘_’) ‘}’
ImportSelector    ::=  id [‘=>’ id | ‘=>’ ‘_’]

Dcl               ::=  ‘val’ ValDcl
                    |  ‘var’ VarDcl
                    |  ‘def’ DefDcl
                    |  ‘type’ {nl} TypeDcl
                    |  INT


ValDcl            ::=  ids ‘:’ Type
VarDcl            ::=  ids ‘:’ Type
DefDcl            ::=  DefSig [‘:’ Type]
DefSig            ::=  id [DefTypeParamClause] DefParamClauses
TypeDcl           ::=  id [TypTypeParamClause] [‘=’ Type]
                    |  id [HkTypeParamClause] TypeBounds

Def               ::=  ‘val’ PatDef
                    |  ‘var’ VarDef
                    |  ‘def’ DefDef
                    |  ‘type’ {nl} TypeDcl
                    |  TmplDef
                    |  INT
PatDef            ::=  Pattern2 {‘,’ Pattern2} [‘:’ Type] ‘=’ Expr
VarDef            ::=  PatDef
                    |  ids ‘:’ Type ‘=’ ‘_’
DefDef            ::=  DefSig [‘:’ Type] ‘=’ Expr
                    |  DefSig [nl] ‘{’ Block ‘}’
                    |  ‘this’ DefParamClause DefParamClauses
                       (‘=’ ConstrExpr | [nl] ConstrBlock)

TmplDef           ::=  ([‘case’] ‘class’ | ‘trait’) ClassDef
                    |  [‘case’] ‘object’ ObjectDef
ClassDef          ::=  id [ClsTypeParamClause]
                       [ConstrMods] ClsParamClauses TemplateOpt
ConstrMods        ::=  AccessModifier
                    |  Annotation {Annotation} (AccessModifier | ‘this’)
ObjectDef         ::=  id TemplateOpt
TemplateOpt       ::=  [‘extends’ Template | [nl] TemplateBody]
Template          ::=  ConstrApps [TemplateBody] | TemplateBody
ConstrApps        ::=  ConstrApp {‘with’ ConstrApp}
ConstrApp         ::=  AnnotType {ArgumentExprs}

ConstrExpr        ::=  SelfInvocation
                    |  ConstrBlock
ConstrBlock       ::=  ‘{’ SelfInvocation {semi BlockStat} ‘}’
SelfInvocation    ::=  ‘this’ ArgumentExprs {ArgumentExprs}

TopStatSeq        ::=  TopStat {semi TopStat}
TopStat           ::=  {Annotation [nl]} {Modifier} TmplDef
                    |  Import
                    |  Packaging
                    |  PackageObject
                    |
Packaging         ::=  ‘package’ QualId [nl] ‘{’ TopStatSeq ‘}’
PackageObject     ::=  ‘package’ ‘object’ ObjectDef

CompilationUnit   ::=  {‘package’ QualId semi} TopStatSeq
```
