" external syntax {{{

	syn include @sql syntax/sql.vim

" }}}

syn case match
syn spell default


" python2 compatibility?
if ! exists('b:python_py2_compat')
  let b:python_py2_compat = 1
endif

" do we want to enable highlighting of any py3 syntax?
let s:python34 = 1
let s:python35 = 1
let s:python36 = 1

if exists('b:python_py3_compat')
  let s:py3 = type(b:python_py3_compat) == type("") ? b:python_py3_compat : ""
  " if the buffer var is a string containing a python version, enable
  " everything up to that version
  if s:py3 == '3.5'
    let s:python36 = 0
  elseif s:py3 == '3.4'
    let s:python36 = 0
    let s:python35 = 0
  elseif s:py3 != '3.6'
    let s:python36 = 0
    let s:python35 = 0
    let s:python34 = 0
  endif
  unlet s:py3
endif


" ERRORS {{{

	" these matches are defined first so that they have minimum priority
	syn cluster pyExpr add=pyCharError,pyMaybeError
	syn match pyCharError /[:;?$)}\]\\%]/ display
	syn match pyMaybeError /['"]/ display
	syn keyword pyKeywordError elseif
	hi! link pyCharError Error
	hi! link pyMaybeError Error
	hi! link pyKeywordError Error

	hi! link pyConditional Conditional

	" certain keywords which cannot appear inside expressions
	syn cluster pyExpr add=pyKeywordError
	syn match pyKeywordError /\<\%(assert\|print\|raise\|return\|break\|continue\|def\|class\)\>/
	hi! link pyKeywordError Error

	syn match pyTabError /\t\+/
	hi! link pyTabError Error

" }}}

" numbers / booleans / None {{{

	syn cluster pyExpr add=pyBoolean,pyNone,pyInteger,pyFloat,pyOctal,pyOctalOld,pyHex

	" True False None
	syn keyword pyNone contained None nextgroup=@pyClOperators,pyCompare skipwhite
	syn keyword pyBoolean True False nextgroup=@pyClOperators,pyCompare skipwhite
	hi! link pyNone Typedef
	hi! link pyBoolean Typedef

	" integers
  " FIXME: '_' is only allowed in 3.6 and higher
	syn match pyInteger /\<\%(0\|[1-9]\%(_\d\|\d\+\)*\)\>/ display nextgroup=@pyClOperators,pyCompare skipwhite
	syn match pyInteger /-[1-9]\%(_\d\|\d\+\)*\>/ display nextgroup=@pyClOperators,pyCompare skipwhite
	hi! link pyInteger Number

	" floats
	syn match pyFloat /-\=\%(0\|[1-9]\%(_\d\|\d\+\)*\)\=\.\d\+\>/ display nextgroup=@pyClOperators,pyCompare skipwhite
	hi! link pyFloat pyInteger

	" octal / hex
  " FIXME: _ is allowed in hex/octal notation as well
	syn match pyOctalOld contained /0\d\+\>/ contains=pyOctalError
	syn match pyOctal contained /0o\d\+\>/ contains=pyOctalError
	syn match pyOctalError contained /[89]\+/
	syn match pyHex contained /0x[0-9a-fA-F]\+/
				\ nextgroup=@pyClOperators,pyCompare skipwhite
	hi! link pyOctal Typedef
	hi! link pyOctalOld IncSearch
	hi! link pyHex Typedef
	hi! link pyOctalError Error

	" broken numbers
	syn keyword pyNumberError 0x
	hi! link pyNumberError Error

" }}}

" '\' - line split {{{

	syn cluster pyClOperators add=pyLineBreakOperators
	syn match pyLineBreakOperators contained /\\$/
				\ nextgroup=@pyClOperators,pyCompare skipnl skipwhite
	hi! link pyLineBreakOperators Statement

	syn cluster pyExpr add=pyLineBreakExpr
	syn match pyLineBreakExpr contained /\\$/
				\ nextgroup=@pyExpr skipnl skipwhite
	hi! link pyLineBreakExpr pyLineBreakOperators

" }}}

syn cluster pyClOperators add=pyItemAccess
syn region pyItemAccess contained keepend extend
			\ contains=@pyExpr,pyItemRange
			\ nextgroup=@pyClOperators skipwhite
			\ matchgroup=pyItemDelim start=/\[/ end=/\]/
			\ matchgroup=Error end=/[)}]/
syn match pyItemRange contained /:/

syn cluster pyClOperators add=pyItemAppendError
syn match pyItemAppendError contained /\[\s*\]/

"syn match pyItemRange contained /-\=\d\+:\%(-\=\d\+\)\=/ contains=pyInteger
hi! link pyItemRange Typedef
hi! link pyItemDelim Delimiter
hi! link pyItemAppendError Error

" auto-set variables
syn cluster pyExpr add=pyConstant
syn keyword pyConstant __name__ __file__ __doc__ __all__
			\ nextgroup=@pyClOperators,pyCompare skipwhite
hi! link pyConstant Operator

" LOOK OUT: this needs lower precedence than all the other known statements
syn match pyIdentifierStatementStart /^\s*\zs\h\w*/ display
			\ contains=pyKnownModule,pySelf
			\ nextgroup=@pyClOperators,pyAnyComma skipwhite
syn match pyIdentifierStatementAfter /\h\w*/ display
			\ contains=pyKnownModule,pyKnownFunction,pySelf
			\ nextgroup=@pyClOperators,pyAnyComma skipwhite
syn match pyAnyComma /,/ display nextgroup=@pyExpr skipwhite skipnl skipempty
hi! link pyAnyComma Macro

" match a variable somewhere
syn cluster pyExpr add=pyIdentifier,pySelf
syn cluster pyClVars add=pyIdentifier,pySelf
syn match pyIdentifier /\<\h\w*\>/ contained display
			\ contains=pyKnownModule,@pyKnownFunctions,pyKeywordError
			\ nextgroup=@pyClOperators skipwhite
syn keyword pySelf contained self
			\ nextgroup=@pyClOperators skipwhite
hi! link pySelf pyClass

syn match pyCloseParenError contained /)/ extend display
hi! link pyCloseParenError Error

" TODO:
"
"   return list(module.__all__)


"syn match pyTopComma /,/ nextgroup=@pyExpr skipwhite
"hi! link pyTopComma Macro

syn cluster pyClOperators add=pyAssign
syn match pyAssign /==\@!/ nextgroup=@pyExpr skipwhite skipnl skipempty
syn match pyAssign #[+-|&^%]=\|//\==\|\*\*\==\|<<=\|>>=# contained nextgroup=@pyExpr skipwhite skipnl skipempty display
hi! link pyAssign Operator

hi! link pyListBrace Typedef
hi! link pyListComma pyListBrace
hi! link pyListCommaError Error

syn cluster pyClOperators add=pyDot
syn match pyDot contained /\./ display
			\ nextgroup=pyProperty,pyMethodCall
hi! link pyDot Operator
syn match pyDotSimple contained /\./ display
hi! link pyDotSimple pyDot

syn match pyProperty contained /\h\w*(\@!/ display
			\ nextgroup=@pyClOperators skipwhite
			\ contains=@pyClAttributes,pyKnownAttribute

" class special properties
syn cluster pyClAttributes add=pySpecialAttribute,pyKnownAttribute
syn keyword pySpecialAttribute contained __class__ __name__ __bases__ __doc__ __module__ __dict__ __slots__ __all__
hi! link pyKnownAttribute Identifier
hi! link pySpecialAttribute Special

syn match pyMethodCall contained /\h\w*\ze\s*(/
			\ contains=@pyClAttributes
			\ nextgroup=pyParamsRegion skipwhite

syn cluster pyExpr add=pyString,pySpecialString,pyRawString
syn region pyString keepend extend oneline
			\ matchgroup=pyStringDelim keepend extend start=/\z(['"]\)/ end=/\z1/ skip=/\\./
			\ contains=@pyClInString
			\ nextgroup=@pyClOperators,pyCompare skipwhite
syn region pyString keepend extend oneline
			\ matchgroup=pyStringDelim keepend extend start=/\z(['"]\)\ze\%(%Y-\)/ end=/\z1/ skip=/\\./
			\ contains=@pyClInDateString
			\ nextgroup=@pyClOperators,pyCompare skipwhite
syn match pySpecialString contained /\(["']\)__main__\1/ display
			\ nextgroup=@pyClOperators,pyCompare skipwhite
hi! link pySpecialString Typedef
hi! link pyStringDelim String
hi! link pyString String

" NOTE: we're going to use python 'raw' strings in double or single quotes for our regex
" strings
syn region pyRawString matchgroup=pyStringDelim start=/\<r\z(\(['"]\)\1\@!\)/ end=/\z1/
			\ contains=@pyClRegex
			\ display keepend extend
			\ nextgroup=@pyClOperators,pyCompare skipwhite
syn region pyRawString matchgroup=pyStringDelim start=/\<r\z("""\|'''\)\ze\s*#\s*REGEX$/ end=/\z1/
			\ contains=@pyClRegex
			\ keepend extend
			\ nextgroup=@pyClOperators,pyCompare skipwhite

syn cluster pyClRegex add=pyRegexEscapeSpecial,pyRegexEscapeError,pyRegexEscapeNormal
syn match pyRegexEscapeError contained /\\./
syn match pyRegexEscapeSpecial contained /\\[AZbBdDsSwW\\]/
syn match pyRegexEscapeNormal contained /\\[()[\]{}.*#+]/
hi! link pyRegexEscapeSpecial Identifier
hi! link pyRegexEscapeNormal Comment
hi! link pyRegexEscapeError Errro

syn cluster pyClRegex add=pyRegexSpecial
syn match pyRegexSpecial contained /[.^$*+?|]/
syn match pyRegexSpecial contained /{\d*,\=\d*}/
hi! link pyRegexSpecial Typedef

syn cluster pyClRegex add=pyRegexRegion,pyRegexGroup
syn region pyRegexRegion contained display
			\ matchgroup=pyRegexGroup start=/(\%(?\%(:\|<\=[=!]\|P<\w\+>\)\)\=/ end=/)/
			\ contains=@pyClRegex
syn region pyRegexRegion contained display
			\ matchgroup=pyRegexGroupConditional start=/(?(\w\+)/ end=/)/
			\ contains=@pyClRegex
syn match pyRegexGroup contained /(?P=\w\+)/
syn match pyRegexGroup contained /\\[1-9]\d*/
hi! link pyRegexGroup Macro
hi! link pyRegexGroupConditional Operator

" regex collections
syn cluster pyClRegex add=pyRegexCollection
syn region pyRegexCollection contained keepend extend
			\ matchgroup=pyRegexCollection start=/\[\^\=\]\=/ end=/\]/
			\ contains=pyRegexCollectionRange
syn match pyRegexCollectionRange contained /[^]]-[^]]/
hi! link pyRegexCollection Function
hi! link pyRegexCollectionRange Identifier

" regex options
syn cluster pyClRegex add=pyRegexOptionContainer
syn match pyRegexOptionContainer contained /(?[A-Za-z]\+)/
			\ contains=pyRegexOption,pyRegexOptionError
syn match pyRegexOptionError contained /[A-Za-z]/
syn match pyRegexOption contained /[iLmsuxt]/
hi! link pyRegexOptionContainer Statement
hi! link pyRegexOptionError Error
hi! link pyRegexOption Typedef

" regex comments
syn cluster pyClRegex add=pyRegexComment
syn region pyRegexComment contained start=/(?#/ end=/)/ contains=@Spell
syn region pyRegexComment contained start=/#/ end=/$/ oneline contains=@Spell
hi! link pyRegexComment Comment

" highlighting for () regions
syn cluster pyExpr add=pyParenRegion
syn region pyParenRegion contained matchgroup=pyParenDelim start=/(/ end=/)/ keepend extend
			\ contains=@pyExpr,pyTupleComma,@pyListComps
			\ nextgroup=@pyClOperators,pyCompare skipwhite
hi! link pyParenDelim Statement

" overriding match for simple tuples
syn match pyTupleComma contained /,/ nextgroup=pyCommaError skipwhite skipnl skipempty display
hi! link pyTupleDelim Typedef
hi! link pyTupleComma Typedef

" formatting and escape characters in strings
syn cluster pyClInString add=pyPrintfKnown,pyPrintfError
syn match pyPrintfError contained /%\%((.\{-})\)\=./ display
syn match pyPrintfKnown contained /%\%((\h\w*)\)\=[disr]/ display contains=pyPrintfKey
syn match pyPrintfKey contained /(\zs\h\w*\ze)/ display
" TODO: check string interpolation in manual
syn match pyPrintfKnown contained /%\%((\h\w*)\)\=+\=0[1-9]\d*d/ display contains=pyPrintfKey
hi! link pyPrintfKnown SpecialChar
hi! link pyprintfKey Identifier
hi! link pyPrintfError Error

syn cluster pyClInDateString add=pyPrintfDateTime,pyPrintfError
syn match pyPrintfDateTime contained /%[aAbBcdHIjmMpSUwWxXyYZ%]/ display
hi! link pyPrintfDateTime Statement

syn cluster pyClInString add=pyStringEscapeKnown,pyStringEscapeError
syn cluster pyClInString add=pyStringEscapeOctal,pyStringEscapeHex,pyStringEscapeHexError
"syn match pyStringEscapeError contained /\\./ display
syn match pyStringEscapeKnown contained /\\[abfnrtv\\"']/ display
syn match pyStringEscapeOctal contained /\\\d\{1,3}/ display
			\ contains=pyOctalError
syn match pyStringEscapeHexError contained /\\x../ display
syn match pyStringEscapeHex contained /\\x[a-f0-9]\{2}/ display
hi! link pyHexError Error
hi! link pyStringEscapeKnown SpecialChar
hi! link pyStringEscapeError Error
hi! link pyStringEscapeOctal pyOctal
hi! link pyStringEscapeHex pyHex
hi! link pyStringEscapeHexError Error

syn cluster pyExpr add=pyBigString
syn region pyBigString keepend extend fold
			\ matchgroup=pyBigStringDelim start=/\c\%(ur\=\)\=\z("""\|'''\)/ end=/\z1/
			\ nextgroup=@pyClOperators,pyCompare skipwhite
			\ contains=@pyClInString,@Spell
syn region pyBigString keepend extend fold transparent
			\ matchgroup=pyBigStringDelim start=/\C\%(ur\=\)\=\z("""\|'''\)\ze\%(CREATE\|INSERT\|UPDATE\|DELETE\|SELECT\|SET\|REPLACE\|DROP\)\>/ end=/\z1/
			\ nextgroup=@pyClOperators,pyCompare skipwhite
			\ contains=@sql,@pyClInString,@Spell
hi! link pyBigString Special
hi! link pyBigStringDelim pyBigString

" match a function call
syn cluster pyExpr add=pyCall
syn match pyCall /\<\h\w*\ze\%(\s*(\)/ contains=@pyKnownFunctions,pySpecialFunction
			\ nextgroup=pyParamsRegion skipwhite
syn region pyParamsRegion contained matchgroup=pyParamsDelim start=/(/ end=/)/ keepend extend
			\ contains=@pyExpr,pyParamsComma,pyParamsUnpack,pyParamsKW
			\ nextgroup=@pyClOperators,pyCompare,pyParamsRegion skipwhite
hi! link pyParamsDelim Delimiter
syn match pyParamsComma contained /,/ display
			\ nextgroup=pyCommaError skipwhite skipnl skipempty

" look for unpack args
syn match pyParamsUnpack contained /\*\*\=,\@!/ display
			\ nextgroup=@Expr,pyCommaError skipwhite
hi! link pyParamsUnpack Macro

" look for kwargs (x=expr)
syn match pyParamsKW contained /\h\w*\s*==\@!/ contains=pySimpleAssign
syn match pySimpleAssign /=/ contained
hi! link pyParamsKW Include
hi! link pySimpleAssign pyAssign

syn cluster pyClStatements add=pyPrintRegionAfter
if b:python_py2_compat
  syn region pyPrintRegionAfter matchgroup=pyPrint start=/\<print\>/ end=/$\|#\@=/ keepend display
        \ contains=@pyExpr,pyPrintComma,pyPrintRedirect
        \ contained oneline
else
  syn region pyPrintRegionAfter matchgroup=pyPrint start=/\<print\s*(/ end=/)/ keepend display
        \ contains=@pyExpr,pyPrintComma
        \ contained oneline
endif


syn region pyIfRegion matchgroup=pyConditional display
			\ start=/^\s*\zsif\>/
			\ start=/^\s*\zselif\>/
			\ end=/:/ keepend extend
			\ nextgroup=@pyClStatements skipwhite
			\ contains=@pyExpr
syn match pyConditional /^\s*\zselse\s*:/ display
syn region pyWhileRegion matchgroup=pyLoop display
			\ start=/^\s*\zswhile\>/
			\ end=/:/ keepend extend
			\ nextgroup=@pyClStatements skipwhite
			\ contains=@pyExpr
hi! link pyLoop Repeat

" assert, print {{{

	syn region pyPrintRegion matchgroup=pyPrint start=/^\s*\zsprint\>/ end=/$\|#\@=/ keepend display
				\ contains=@pyExpr,pyPrintComma,pyPrintRedirect
	syn match pyPrintRedirect contained />>/ nextgroup=@pyExpr skipwhite
	syn match pyPrintComma contained /,/ nextgroup=pyCommaError skipwhite skipnl skipempty display
	hi! link pyPrintComma pyPrint
	hi! link pyPrintRedirect pyPrint
	hi! link pyPrint Macro

	syn region pyAssertRegion keepend display
				\ matchgroup=pyAssert start=/\<assert\>/ end=/$\|#\@=/ keepend display
				\ contains=@pyExpr,pyAssertComma
	syn match pyAssertComma contained /,/ nextgroup=pyCommaError skipwhite skipnl skipempty display
	hi! link pyAssert Macro
	hi! link pyAssertComma pyAssert

" }}}

" decorators {{{

	syn cluster pyClStatements add=pyDecorator
	syn match pyDecorator /^\s*@/ nextgroup=@pyExpr
	hi! link pyDecorator Statement



" }}}

" return/break/continue/raise/pass/exit() {{{

	syn cluster pyClStatements add=pyControl
	syn keyword pyControl break continue pass
	hi! link pyControl Statement

	syn cluster pyClStatements add=pyStatementRegion,pyExitRegion
	syn region pyStatementRegion keepend
				\ matchgroup=pyStatement start=/\<\%(return\|raise\)\>/ end=/$\|#\@=/
				\ contains=@pyExpr,pyStatementComma
	syn match pyStatementComma contained /,/ nextgroup=pyCommaError skipwhite skipnl skipempty display
	hi! link pyStatementComma pyStatement
	hi! link pyStatement Statement

	syn cluster pyExpr add=pyExitRegion
	syn region pyExitRegion matchgroup=pyStatement start=/\<exit(/ end=/)/ keepend extend
				\ contains=@pyExpr

" }}}

" del, print {{{

	syn region pyDelRegion keepend display
				\ matchgroup=pyDel start=/\<del\>/ end=/$\|#\@=/
				\ contains=@pyClVars,pyComment,pyDelComma
	syn match pyDelComma contained /,/ nextgroup=pyCommaError skipwhite skipnl
	hi! link pyDelComma pyDel
	hi! link pyDel Operator

	syn region pyGlobalRegion keepend display
				\ matchgroup=pyGlobal start=/\<global\>/ end=/$\|#\@=/
				\ contains=@pyClVars,pyComment,pyGlobalComma
	syn match pyGlobalComma contained /,/ nextgroup=pyCommaError skipwhite skipnl
	hi! link pyGlobalComma pyGlobal
	hi! link pyGlobal pyDel



" }}}

" {{{ format-strings

  if s:python36
    syn match pyFStringStart /[fF]\ze['"]/ contained nextgroup=pyFString
    syn match pyFStringStart /\%([Ff]r\|r[fF]\)\ze['"]/ contained nextgroup=pyFStringRaw
    syn region pyFString start=/\z(['"]\{1,3}\)/ end=/\z1/ contained keepend extend
          \ contains=pyFStringExpr,pyFStringEscape,pyFStringEscapeError
    syn region pyFStringRaw start=/\z(['"]\{1,3}\)/ end=/\z1/ contained keepend extend
          \ contains=pyFStringExpr
    syn cluster pyExpr add=pyFStringStart
    
    " any unrecognised '\<something>' is a warning in 3.6, possibly an error
    " in future versions
    syn match pyFStringEscapeError contained /\\./ contains=pyFStringEscapeBadDot transparent
    syn match pyFStringEscapeBadDot contained /\\/
    hi! link pyFStringEscapeBadDot IncSearch

    syn match pyFStringEscape contained /\\\%($\|[\\'"abfnrtv]\|o\o\o\o\|x\x\x\)/
    syn match pyFStringEscape contained /{{\|}}/
    hi! link pyFStringEscape SpecialChar

    " NOTE: this match is for the new python 3.6 f-strings since it accepts an
    " expr, not just a placeholder
    syn region pyFStringExpr matchgroup=pyFStringBrace start=/{{\@!/ end=/}/ contained keepend extend
          \ contains=@pyExpr,pyFMTConversion,pyClFMTSpec

    hi! link pyFStringStart Delimiter
    hi! link pyFString pyString
    hi! link pyFStringBrace Delimiter
  endif

  " type conversion
  syn match pyFMTConversion contained /![rsa]/ nextgroup=pyClFMTSpec
  hi! link pyFMTConversion Operator

  syn region pyClFMTSpec matchgroup=pyFMTSpecDelim start=/:/ end=/\ze}/ contained oneline keepend extend
        \ contains=pyFMTSign,pyFMTAltForm,pyFMTZero,pyFMTWidthGroupingPrecisionType,pyFMTAlign,pyFStringExpr
  hi! link pyFMTSpecDelim Function
  " give everything in the format spec IncSearch highlighting ... if the
  " various parts of the format spec are syntactically correct then the other
  " bits below will override IncSearch with correct colours
  hi! link pyClFMTSpec IncSearch

  " format spec
  syn match pyFMTSign contained /[ +-]/
  syn match pyFMTAltForm contained /#/

  " the width/grouping/precision all in one
  syn match pyFMTWidthGroupingPrecisionType contained /[0-9_]*[,_]\=\%(\.[0-9_]\+\)\=[a-zA-Z%]\ze}/ display
        \ contains=pyFMTType,pyFMTTypeError
  syn match pyFMTZero contained /0/ contained nextgroup=pyFMTWidthGroupingPrecisionType
  hi! link pyFMTZero pyFMTAltForm

  syn match pyFMTTypeError contained /[a-zA-Z]/
  hi! link pyFMTTypeError IncSearch
  syn match pyFMTType contained /s/
  syn match pyFMTType contained /[bcdoxXn]/
  syn match pyFMTType contained /[eEfFgGn%]/
  syn match pyFMTGrouping contained /,/
  if s:python36
    syn match pyFMTGrouping contained /_/
  else
    syn match pyFMTGroupingError contained /_/
    hi! link pyFMTGroupingError Error
  endif

  " the 'align' needs to come last since it could start with literally any
  " character
  syn match pyFMTAlign contained /.\=[<>=^]/

  hi! link pyFMTSign Statement
  hi! link pyFMTSign Statement
  hi! link pyFMTAltForm Statement
  hi! link pyFMTAlign Macro
  hi! link pyFMTWidthGroupingPrecisionType Number
  hi! link pyFMTType Type
" }}}

if s:python35
  syn keyword pyAsyncDef async
  hi! link pyAsyncDef pyDef
else
  syn keyword pyAsyncError async
  syn cluster pyExpr add=pyAsyncError
  hi! link pyAsyncError pyAwaitError
endif

syn region pyDefRegion matchgroup=pyDef start=/\<def\>/ end=/:/ keepend extend
			\ nextgroup=@pyClStatements skipwhite
			\ contains=pyDefParams,@pyClAttributes
hi! link pyDef Keyword

syn region pyDefParams contained matchgroup=pyDefDelim start=/(/ end=/)/ keepend extend
				\ contains=pyDefParam,pyDefParamTuple,pySelf,pyDefComma,pyParamsUnpack,pyComment,pyDefKwargSep
hi! link pyDefDelim Macro
syn match pyDefParam contained /\<\h\w*\>/ display
			\ nextgroup=pyDefParamDefault skipwhite
syn region pyDefParamTuple contained matchgroup=pyParenDelim start=/(/ end=/)/ keepend extend
			\ contains=pyTupleComma
syn match pyDefParamDefault contained /=/ nextgroup=@pyExpr skipwhite display
hi! link pyDefParamDefault Operator

syn match pyDefComma contained /,/ display
			\ nextgroup=pyCommaError skipwhite skipnl skipempty
hi! link pyDefComma pyDef

syn match pyDefKwargSep contained /\*,\@=/
if b:python_py2_compat
  " if we need to be python2-compatible, make the kwarg separator an error
  hi! link pyDefKwargSep Error
else
  hi! link pyDefKwargSep pyDefComma
endif

syn keyword pyFrom from nextgroup=pyFromModule skipwhite
syn region pyImportRegion matchgroup=pyFrom start=/\<import\>/ end=/$\|#\@=/
			\ contains=pyKnownIdentifier,pyImportModule,pyImportComma,pyModuleDot,pyComment
syn region pyImportRegion matchgroup=pyFrom start=/\<import\s*(/ end=/)/
			\ contains=pyKnownIdentifier,pyImportModule,pyImportComma,pyComment
			\ matchgroup=Error end=/[\]}]/
"syn keyword pyImport import as nextgroup=@pyClModules skipwhite
hi! link pyImport Macro
hi! link pyFrom pyImport

syn match pyFromModule contained /\*\|\h\w*\%(\.\h\w*\)*/
			\ contains=pyKnownModule,pyModuleDot
			\ nextgroup=pyImportRegion skipwhite
syn match pyImportModule contained /\h\w*/
			\ contains=pyKnownModule,@pyClasses
			\ nextgroup=pyImportAs,pyImportComma skipwhite
syn match pyModuleDot contained /\./
hi! link pyModuleDot pyFrom
syn keyword pyImportAs contained as
syn match pyImportComma contained /,/
			\ nextgroup=pyCommaError skipwhite skipnl skipempty
hi! link pyImportComma pyFrom
hi! link pyImportAs pyFrom

syn cluster pyClModules add=pyKnownModule
syn keyword pyKnownModule contained __builtin__ i18n node
			\ httplib urllib2
			\ zlib gzip
			\ UserDict
hi! link pyKnownModule Function
syn cluster pyClModules add=pyAllModules
syn match pyAllModules contained /\*/ display
hi! link pyAllModules Function

syn cluster pyExpr add=pyKnownIdentifier
hi! link pyKnownIdentifier Function

" building a modules tree:
if ! exists('s:modules')
  let s:modules = {}
endif
let s:modules[bufnr('')] = {}
function! <SID>AddModule(name, properties) " {{{
  let l:bufmodules = s:modules[bufnr('')]
  if ! get(l:bufmodules, a:name, 0)
    let s:modules[bufnr('')][a:name] = 1
    " add a keyword for the module name itself
    exe printf('syn keyword pyKnownIdentifier %s nextgroup=pyDot_%s,@pyClOperators,pyCompare,pyParamsRegion skipwhite', a:name, a:name)
    " add a dot match for that module
    exe printf('syn match pyDot_%s contained /\./ nextgroup=pyInside_%s', a:name, a:name)
    " link colours
    exe printf('hi! link pyDot_%s pyDot', a:name)
    exe printf('hi! link pyInside_%s pyKnownIdentifier', a:name)
  endif

  " add [more] properties of that module
  exe printf('syn keyword pyInside_%s contained %s nextgroup=@pyClOperators,pyCompare,pyParamsRegion skipwhite', a:name, a:properties)
endfunction " }}}
function! <SID>AddSubModule(owner, name, properties) " {{{
	let l:inside = printf('pyInside_%s_%s', a:owner, a:name)
	let l:dot = printf('pyDot_%s_%s', a:owner, a:name)
	exe printf('syn keyword pyInside_%s contained %s nextgroup=%s,@pyClOperators,pyCompare,pyParamsRegion skipwhite', a:owner, a:name, l:dot)
	" add a dot match for that module
	exe printf('syn match %s contained /\./ nextgroup=%s', l:dot, l:inside)
	" add properties of that module
	exe printf('syn keyword %s contained %s nextgroup=@pyClOperators,pyCompare,pyParamsRegion skipwhite', l:inside, a:properties)
	" link colours
	exe printf('hi! link %s pyDot', l:dot)
	exe printf('hi! link %s pyKnownIdentifier', l:inside)
endfunction " }}}

" sys
" TODO: all of sys!
call <SID>AddModule('sys', 'argv stdin path')
for s:pipe in [ 'stderr', 'stdout' ]
	call <SID>AddSubModule('sys', s:pipe, 'write')
endfor

if s:python35
  call <SID>AddModule('typing', 'Any Union TypeVar Generic NewType Type Iterable Iterator Reversible SupportsInt SupportsFloat SupportsComplex SupportsBytes SupportsAbs SupportsRound Container Hashable Sized AbstractSet MutableSet Mapping MutableMapping Sequence MutableSequence ByteString List Set ForzenSet MappingView KeysView ItemsView ValuesView Awaitable Coroutine AsyncIterable AsyncIterator Dict DefaultDict Generator AsyncGenerator Text io re NamedTuple cast get_type_hints Optional Tuple Callable ClassVar TYPE_CHECKING')
  " these ones are decorators
  call <SID>AddModule('typing', 'overload no_type_check no_type_check_decorator')
  if s:python36
    " these were added in 3.6
    call <SID>AddModule('typing', 'Collection Deque ContextManager Counter ChainMap')
  endif
endif

" types
call <SID>AddModule('types', 'NoneType TypeType ObjectType IntType LongType FloatType BooleanType ComplexType StringType UnicodeType, StringTypes BufferType TupleType ListType DictType FunctionType LambdaType CodeType GeneratorType ClassType UnboundMethodType InstanceType MethodType BuiltinFunctionType BuiltinMethodType ModuleType FileType XRangeType TracebackType FrameType SliceType EllipsisType DictProxyType NotImplementedType GetSetDescriptorType MemberDescriptorType')

" types
call <SID>AddModule('exceptions', 'Warning UserWarnign DeprecationWarning SyntaxWarning RuntimeWarning FutureWarning PendingDeprecationWarning ImportWarning UnicodeWarning')

" warnings
call <SID>AddModule('warnings', 'warn warn_explicit showwarning formatwarning filterwarnings simplefilter resetwarnings')

" hashlib
call <SID>AddModule('hashlib', 'new md5 sha1 sha224 sha256 sha384 sha512')

" time, timeit
" TODO: all of 'time' module
call <SID>AddModule('time', 'accept2dyear altzone asctime clock ctime daylight gmtime localtime mktime sleep strftime strptime struct_time time timezone tzname tzset')
call <SID>AddModule('timeit', 'Timer')

" random
let s:random_methods = 'seed random uniform randint choice randrange sample shuffle normalvariate lognormvariate expovariate vonmisesvariate gammavariate gauss betavariate paretovariate weibullvariate getstate setstate jumpahead getrandbits'
call <SID>AddModule('random', s:random_methods . ' Random WichmannHill SystemRandom')
" methods of random
execute 'syn keyword pyKnownMethod contained' s:random_methods


" os
let s:os =  "name curdir pardir sep extsep altsep pathsep linesep defpath devnull unlink system"
			\ . " environ chdir fchdir getcwd ctermid getegid geteuid getgid getgroups getlogin getpgid getpgrp"
			\ . " getpid getppid getuid getenv putenv setegid seteuid setgid setgroups setpgid setreuid setregid setsid setuid"
			\ . " strerror umask uname unsetenv"
call <SID>AddModule('os', s:os)
let s:posixpath = "normcase isabs join splitdrive split splitext basename dirname commonprefix getsize getmtime getatime getctime"
			\ . " islink exists lexists isdir isfile ismount walk expanduser expandvars normpath abspath samefile sameopenfile samestat"
      \ . " curdir pardir sep pathsep defpath altsep extsep devnull realpath supports_unicode_filenames"
call <SID>AddSubModule('os', 'path', s:posixpath)

call <SID>AddModule('getopt', 'getopt gnu_getopt')

" math"
call <SID>AddModule('math', 'e pi ceil fabs floor fmod frexp ldexp modf exp log log10 pow sqrt acos asin atan atan2 cos hypot sin tan degrees radians cosh sinh tanh')

" array
call <SID>AddModule('array', 'array')

" atexit
call <SID>AddModule('atexit', 'register')

" urllib, sgmllib
call <SID>AddModule('urllib', "urlopen URLopener FancyURLopener urlretrieve urlcleanup quote quote_plus unquote unquote_plus urlencode url2pathname pathname2url splittag localhost thishost ftperrors basejoin unwrap splittype splithost splituser splitpasswd splitport splitnport splitquery splitattr splitvalue splitgophertype getproxies")
call <SID>AddModule('sgmllib', "SGMLParser SGMLParseError")

" glob
call <SID>AddModule('glob', 'glob iglob')

" StringIO
call <SID>AddModule('StringIO', 'StringIO')

" re
call <SID>AddModule('re', 'match search sub subn split findall compile purge template escape IGNORECASE LOCALE MULTILINE DOTALL VERBOSE UNICODE error')

" socket
call <SID>AddModule('socket', 'socket socketpair fromfd gethostname gethostbyname gethostbyaddr getservbyname getprotobyname ntohs ntohl htons htonl inet_aton inet_ntoa ssl getdefaulttimeout setdefaulttimeout')

" shutil
call <SID>AddModule('shutil', 'copyfileobj copyfile copymode copystat copy copy2 ignore_patterns copytree rmtree move Error')

" gc
let s:gc = "enable disable isenabled collect set_debug get_debug get_objects set_threshold"
			\ . " get_count get_threshold get_referrers get_referents garbage"
			\ . " DEBUG_STATS DEBUG_COLLECTABLE DEBUG_UNCOLLECTABLE DEBUG_INSTANCES DEBUG_OBJECTS"
			\ . " DEBUG_SAVEALL DEBUG_LEAK"
call <SID>AddModule('gc', s:gc)

" weakref
let s:weakref = 'WeakKeyDictionary WeakValueDictionary'
			\ . ' ref proxy getweakrefcount getweakrefs'
			\ . ' ReferenceType ProxyType CallableProxyType ProxyTypes ReferenceError'

" other:
call <SID>AddModule('pygame', 'QUIT ACTIVEEVENT KEYDOWN KEYUP MOUSEMOTION MOUSEBUTTONUP MOUSEBUTTONDOWN JOYAXISMOTION JOYBALLMOTION JOYHATMOTION JOYBUTTONUP JOYBUTTONDOWN VIDEORESIZE VIDEOEXPOSE USEREVENT')
call <SID>AddSubModule('pygame', 'event', 'pump get poll wait peek clear event_name set_blocked set_allowed get_blocked set_grab get_grab post Event')
call <SID>AddSubModule('pygame', 'mixer', 'init pre_init quit get_init stop pause unpause fadeout set_num_channels get_num_channels set_reserved find_channel get_busy Sound Channel')

" subprocess
call <SID>AddModule('subprocess', 'PIPE STDOUT Popen call check_call CalledProcessError')
syn keyword pyKnownMethod contained poll wait communicate send_signal terminate kill stdin stdout stderr pid returncode

" json/pickle/cPickle
call <SID>AddModule('json', 'dump dumps load loads JSONDecoder JSONEncoder')
let s:pickle = 'dump dumps load loads Pickler Unpickler HIGHEST_PROTOCOL PickleError PicklingError UnpicklingError UnpickleableError format_version compatible_formats BadPickleGet'
call <SID>AddModule('pickle', s:pickle)
call <SID>AddModule('cPickle', s:pickle)

syn cluster pyExpr add=pyAndOr
syn cluster pyClOperators add=pyAndOr
syn keyword pyAndOr contained and or
			\ nextgroup=@pyExpr skipwhite
hi! link pyAndOr Operator

syn cluster pyClOperators add=pyMath
syn match pyMath contained #\%(\*\*\=\|//\=\|[+\-]\)=\@!# display
			\ nextgroup=@pyExpr skipwhite
hi! link pyMath Operator

syn cluster pyClOperators add=pyBitwise
syn match pyBitwise contained /[&|\^~]=\@!\|<<\|>>/ display
			\ nextgroup=@pyExpr skipwhite
hi! link pyBitwise pyMath

syn cluster pyClOperators add=pyPrintf
syn match pyPrintf contained /%/ display
			\ nextgroup=@pyExpr skipwhite
hi! link pyPrintf Operator

" lambda functions
syn cluster pyExpr add=pyLambdaRegion,pyLambdaError
syn region pyLambdaRegion contained matchgroup=pyLambda start=/\<lambda\>/ end=/:/ display
			\ matchgroup=Error end=/,\s*:/
			\ nextgroup=@pyExpr skipwhite
			\ contains=pyLambdaComma,pyParamsUnpack
hi! link pyLambda Macro

" this error version takes precedence
syn match pyLambdaError contained /\<lambda[^a-z ]\+:/ display
hi! link pyLambdaError Error

syn match pyLambdaComma contained /,/ display
			\ nextgroup=pyCommaError,pyColonError skipwhite
hi! link pyLambdaComma pyLambda

" where certain characters shouldn't appear
syn match pyCommaError contained /,/ extend display
syn match pyColonError contained /:/ extend display
hi! link pyCommaError Error
hi! link pyColonError Error

" these have to be contained inside a match first
syn cluster pyKnownFunctions add=pyKnownFunction,pySpecialFunction
" basic types
syn keyword pyKnownFunction contained type bool int long float complex str unicode tuple list dict object file set
				\ classmethod staticmethod property iter super
" testing values
syn keyword pyKnownFunction contained isinstance issubclass all any cmp callable
" exploring
syn keyword pyKnownFunction contained getattr hasattr setattr delattr dir len hash id
" mathematical
syn keyword pyKnownFunction contained max min sum abs divmod pow round
" exceptions
syn keyword pyKnownFunction contained Warning UserWarning DeprecationWarning SyntaxWarning RuntimeWarning FutureWarning PendingDeprecationWarning ImportWarning UnicodeWarning
" other functions
syn keyword pyKnownFunction contained
				\ Exception
				\ chr unichr ord hex oct
				\ reversed sorted slice zip map filter reduce range xrange
				\ compile repr open enumerate execfile frozenset help input raw_input reload vars
" special
syn keyword pySpecialFunction contained eval locals globals
hi! link pyKnownFunction Function
hi! link pySpecialFunction Identifier

" list
syn cluster pyExpr add=pyList
syn region pyListAssignRegion keepend extend
			\ matchgroup=pyListAssignBrace
			\ start=/\[/ end=/\]/
			\ contains=pyListAssignComma
			\ nextgroup=pyAssign skipwhite
syn region pyListAssignRegion keepend extend
			\ matchgroup=pyListAssignBrace
			\ start=/(/ end=/)/
			\ contains=pyListAssignComma
			\ nextgroup=pyAssign skipwhite
syn match pyListAssignComma contained /,/ display
			\ nextgroup=pyCommaError,pyColonError skipwhite
hi! link pyListAssignComma Macro
hi! link pyListAssignBrace Macro

syn region pyList contained matchgroup=pyListBrace start=/\[/ end=/\]/ keepend extend
			\ contains=@pyListComps,@pyExpr,pyListComma
			\ nextgroup=@pyClOperators,pyCompare skipwhite
			\ matchgroup=Error end=/[)]]/
syn match pyListComma contained /,/ nextgroup=pyListCommaError skipwhite skipnl display
syn match pyListCommaError contained /,/ display

syn cluster pyListComps add=pyListCompRegion
syn region pyListCompRegion contained keepend extend
			\ matchgroup=pyListComp start=/\<\%(async\_s\+\)\=for\>/ end=/\<in\>/
			\ contains=pyListCompComma
"syn region pyListFilterRegion contained keepend extend display
"			\ start=/\<\zefor\>/
"			\ matchgroup=pyListComp end=/\<if\>/ end=/]/
"			\ contains=pyListCompRegion
syn match pyListCompComma contained /,/ nextgroup=pyCommaError skipwhite skipnl
hi! link pyListCompComma pyListComp
hi! link pyListComp Typedef

syn cluster pyClOperators add=pyIfExprRegion
syn region pyIfExprRegion contained matchgroup=pyIfExpr start=/\<if\>/ end=/\<else\>/ display
			\ contains=@pyExpr
			\ nextgroup=@pyExpr skipwhite
hi! link pyIfExpr Operator

" dictionary
syn cluster pyExpr add=pyDict
syn region pyDict matchgroup=pyDictBrace start=/{/ end=/}/ keepend extend
			\ contains=pyDictComma,@pyExpr,pyDictKeyString,pyDictColon,pyComment,@pyListComps
" regular string
syn region pyDictKeyString contained keepend extend display oneline
			\ matchgroup=pyDictKey start=/\z(['"]\)/ end=/\z1\s*:/ skip=/\\./
			\ contains=@pyClInString
			\ nextgroup=@pyExpr skipwhite
" RAW version - no escape characters
syn region pyDictKeyString contained keepend extend display oneline
			\ matchgroup=pyDictKey start=/r\z(['"]\)/ end=/\z1\s*:/ skip=/\\./
			\ nextgroup=@pyExpr skipwhite
hi! link pyDictKeyString pyDictKey
syn match pyDictColon contained /:/ nextgroup=pyCommaError skipwhite skipnl display
hi! link pyDictColon pyDictKey
syn match pyDictComma contained /,/ nextgroup=pyCommaError skipwhite skipnl display
hi! link pyDictBrace Typedef
hi! link pyDictKey Number
hi! link pyDictComma pyDictBrace
hi! link pyDictError Error

" with {{{

	syn region pyWithRegion keepend extend
				\ matchgroup=pyWith end=/:/ start=/^\s*with\>/
				\ contains=pyWithInner,@pyExpr
  syn keyword pyWithInner as contained nextgroup=pyIdentifier
  syn match pyWithInner /,/ contained
"	syn region pyWithInner contained keepend extend matchgroup=pyWith start=/^\s*\zswith\>/ end=/\<as\>/
"				\ contains=@pyExpr
	hi! link pyWith Keyword
	hi! link pyWithInner pyWith

" }}}

syn region pyForRegion keepend extend
			\ start=/^\s*\zefor\>/
			\ matchgroup=pyLoop end=/:/
			\ contains=pyForInner,@pyExpr
syn region pyForInner contained keepend extend matchgroup=pyLoop start=/^\s*\zsfor\>/ end=/\<in\>/
			\ contains=pyLoopComma,pyLoopTupleRegion
syn region pyLoopTupleRegion contained matchgroup=pyLoopTuple start=/(/ end=/)/
			\ contains=pyLoopComma
syn match pyLoopComma contained /,/ nextgroup=pyCommaError skipwhite skipnl
hi! link pyLoopComma pyLoop
hi! link pyLoopTuple pyLoop
hi! link pyLoop Repeat

"syn match pyListFilter contained /\<if\>/
"hi! link pyListFilter Typedef

" the 'not' operator and the '-' operator
syn cluster pyExpr add=pyNot,pyUnary,pyYield
syn cluster pyClStatements add=pyYield
syn match pyNot contained /\<not\>\%(\_s*in\)\@!/ nextgroup=pyCompareIn,@pyExpr skipwhite skipnl
syn match pyYield /\<yield\>\%(\_s*from\>\)\=/ nextgroup=@pyExpr skipwhite

if s:python35
  syn keyword pyAwait await nextgroup=@pyExpr skipwhite
  if s:python36
    syn cluster pyExpr add=pyAwait
  endif
  hi! link pyAwait pyYield
else
  syn keyword pyAwaitError await
  syn cluster pyExpr add=pyAwaitError
  hi! link pyAwaitError Error
endif

syn match pyUnary contained /[\-+~]\d\@!/
hi! link pyNot Operator
hi! link pyYield Statement
hi! link pyUnary Operator

" array methods:
" from apihelper import info
" li = []
" info(li)

" global functions:
"		type()
"		str()
" 	dir() <- very cool
" 	callable()
" 	getattr(obj, prop, default) <- equivalent to PHP: $obj->$prop

syn cluster pyClAttributes add=pyKnownMethod,pySpecialMethod
syn keyword pyKnownMethod contained extend
			\ seek read open close append write
			\ join split replace strip upper lower
			\ items values keys get has_key
			\ clear update
			\ isalnum isalpha

" special method names
syn keyword pySpecialMethod contained
			\ __new__ __init__ __del__
			\ __str__ __repr__ __unicode__ __hash__
			\ __len__ __nonzero__
			\ __getitem__ __setitem__ __delitem__
			\ __cmp__ __rcmp__ 
			\ __lt__ __le__ __eq__ __ne__ __gt__ __ge__
			\ __add__ __iadd__ __sub__ __isub__
			\ __contains__ __iter__
			\ __call__
			\ __getattr__ __setattr__ __delattr__ __getattribute__
			\ __get__ __set__ __delete__
if s:python36
  " PEP487
  syn keyword pySpecialMethod contained
        \ __init_subclass__ __set_name__
endif

hi! link pyKnownMethod Function
hi! link pySpecialMethod SpecialChar

syn region pyClassRegion matchgroup=pyClass start=/^\s*class\>/ end=/:/
			\ contains=pyKnownModule,@pyClasses,pyClassParentsRegion
syn region pyClassParentsRegion contained matchgroup=pyclass start=/(/ end=/)/ keepend extend
			\ contains=@pyClModules,@pyClasses
hi! link pyClass Typedef

syn cluster pyClasses add=pyKnownClass
syn keyword pyKnownClass contained UserDict IterableUserDict dict
hi! link pyKnownClass Function


" TODO:
" global? with? exec?

" Comparison operators {{{

syn cluster pyClOperators add=pyCompare,pyCompareIn,pyCompareIs
syn match pyCompare contained /[!=]=\|<>\|[<>]=\=/ display
			\ nextgroup=@pyExpr skipwhite
syn match pyCompareIn contained /\<not\s\+in\>/ display
			\ nextgroup=@pyExpr skipwhite
syn keyword pyCompareIn contained in
			\ nextgroup=@pyExpr skipwhite
syn match pyCompareIs contained /\<is\%(\s\+not\|\>\)/
			\ nextgroup=@pyExpr skipwhite
hi! link pyCompareIs pyCompare
hi! link pyCompareIn pyCompare
hi! link pyCompare SpecialChar

" }}}

" exceptions {{{

	syn match pyTry /^\s*\zs\%(try\|finally\)\s*:/ display
	hi! link pyTry Statement

	syn region pyExceptRegion matchgroup=pyTry start=/^\s*\zsexcept\>/ end=/:/
				\ contains=pyKnownModule,pyKnownFunction,pyExceptModulesRegion,pyExceptComma,pyExceptAs
				\ matchgroup=Error end=/[}\];]/
	syn region pyExceptModulesRegion contained keepend extend display
				\ matchgroup=pyTry start=/(/ end=/)/
				\ contains=pyKnownModule,pyKnownFunction,pyExceptComma
				\ matchgroup=Error end=/[}\]:;]/

	syn match pyExceptComma contained /,/ display
				\ nextgroup=pyCommaError skipwhite skipnl
  syn keyword pyExceptAs contained as nextgroup=pyCommaError skipwhite skipnl
	hi! link pyExceptComma pyTry
	hi! link pyExceptAs pyTry

" }}}

syn cluster pyExpr add=pyComment
syn region pyComment start=/#/ end=/$/ oneline keepend extend contains=@Spell
"syn region pyComment contained start=/^\s*#'/ end=/$/ oneline
hi! link pyComment Comment

" folding regions (def, class) {{{

  if 0
    syn region pyFoldRegion fold
          \ start=/^\ze\z(\s*\)\%(def\|class\)\>/
          \ skip=/\n\s*\%(#\|$\)/
          \ end=/\ze\n\+\%(\z1\s\)\@!/
          \ keepend extend
          \ contains=pyDefRegion,pyClassRegion,@pyCl
  endif

" }}}

" syncing {{{

	syn sync minlines=80

	" any # at the start of the line is considered to be a comment
	syn sync match pySynComment groupthere NONE /^#.\+$/

	" this would be invalid syntax for a string start, so it must be the end of a string
"	syn sync match pyStringEnd grouphere NONE /\%([?)\]}.&$%@!~]\|\w\{3,}\)\s*"""$/

	" the start of a doc string - triple-quote followed by at least 2 words without
	" any punctuation
"	syn sync match pyStringStart groupthere pyBigString /^\s*[a-z]\{0,2}"""\%(\w\+\s\+\)\{2}/

" }}}

" prevents other syntax files from loading
let b:current_syntax = "python"

" vim: foldmethod=marker
