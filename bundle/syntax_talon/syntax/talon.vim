" Vim syntax file
" Language:		talon
" Maintainer:		Doug Kearns <dougkearns@gmail.com>
" URL:			http://vim-talon.talonforge.org
" Anon CVS:		See above site
" Release Coordinator:	Doug Kearns <dougkearns@gmail.com>
" ----------------------------------------------------------------------------
"
" Previous Maintainer:	Mirko Nasato
" Thanks to perl.vim authors, and to Reimer Behrends. :-) (MN)
" ----------------------------------------------------------------------------

if exists("b:current_syntax")
  finish
endif

if has("folding") && exists("talon_fold")
  setlocal foldmethod=syntax
endif

syn cluster talonNotTop contains=@talonExtendedStringSpecial,@talonDeclaration,talonConditional,talonExceptional,talonMethodExceptional,talonTodo

if exists("talon_space_errors")
  if !exists("talon_no_trail_space_error")
    syn match talonSpaceError display excludenl "\s\+$"
  endif
  if !exists("talon_no_tab_space_error")
    syn match talonSpaceError display " \+\t"me=e-1
  endif
endif

" Operators
if exists("talon_operators")
  syn match  talonOperator "[~!^&|*/%+-]\|\%(class\s*\)\@<!<<\|<=>\|<=\|\%(<\|\<class\s\+\u\w*\s*\)\@<!<[^<]\@=\|===\|==\|=\~\|>>\|>=\|=\@<!>\|\*\*\|\.\.\.\|\.\.\|::"
  syn match  talonOperator "->\|-=\|/=\|\*\*=\|\*=\|&&=\|&=\|&&\|||=\||=\|||\|%=\|+=\|!\~\|!="
  syn region talonBracketOperator matchgroup=talonOperator start="\%(\w[?!]\=\|[]})]\)\@<=\[\s*" end="\s*]" contains=ALLBUT,@talonNotTop
endif

" Expression Substitution and Backslash Notation
syn match talonStringEscape "\\\\\|\\[abefnrstv]\|\\\o\{1,3}\|\\x\x\{1,2}"						    contained display
syn match talonStringEscape "\%(\\M-\\C-\|\\C-\\M-\|\\M-\\c\|\\c\\M-\|\\c\|\\C-\|\\M-\)\%(\\\o\{1,3}\|\\x\x\{1,2}\|\\\=\S\)" contained display
syn match talonQuoteEscape  "\\[\\']"											    contained display

syn region talonInterpolation	      matchgroup=talonInterpolationDelimiter start="#{" end="}" contained contains=ALLBUT,@talonNotTop
syn match  talonInterpolation	      "#\%(\$\|@@\=\)\w\+"    display contained contains=talonInterpolationDelimiter,talonInstanceVariable,talonGlobalVariable
syn match  talonInterpolationDelimiter "#\ze\%(\$\|@@\=\)\w\+" display contained
syn match  talonInterpolation	      "#\$\%(-\w\|\W\)"       display contained contains=talonInterpolationDelimiter,talonInvalidVariable
syn match  talonInterpolationDelimiter "#\ze\$\%(-\w\|\W\)"    display contained
syn region talonNoInterpolation	      start="\\#{" end="}"            contained
syn match  talonNoInterpolation	      "\\#{"		      display contained
syn match  talonNoInterpolation	      "\\#\%(\$\|@@\=\)\w\+"  display contained
syn match  talonNoInterpolation	      "\\#\$\W"		      display contained

syn match talonDelimEscape	"\\[(<{\[)>}\]]" transparent display contained contains=NONE

syn region talonNestedParentheses    start="("  skip="\\\\\|\\)"  matchgroup=talonString end=")"	transparent contained
syn region talonNestedCurlyBraces    start="{"  skip="\\\\\|\\}"  matchgroup=talonString end="}"	transparent contained
syn region talonNestedAngleBrackets  start="<"  skip="\\\\\|\\>"  matchgroup=talonString end=">"	transparent contained
syn region talonNestedSquareBrackets start="\[" skip="\\\\\|\\\]" matchgroup=talonString end="\]"	transparent contained

syn cluster talonStringSpecial	      contains=talonInterpolation,talonNoInterpolation,talonStringEscape
syn cluster talonExtendedStringSpecial contains=@talonStringSpecial,talonNestedParentheses,talonNestedCurlyBraces,talonNestedAngleBrackets,talonNestedSquareBrackets

" Numbers and ASCII Codes
syn match talonASCIICode	"\%(\w\|[]})\"'/]\)\@<!\%(?\%(\\M-\\C-\|\\C-\\M-\|\\M-\\c\|\\c\\M-\|\\c\|\\C-\|\\M-\)\=\%(\\\o\{1,3}\|\\x\x\{1,2}\|\\\=\S\)\)"
syn match talonInteger	"\%(\%(\w\|[]})\"']\s*\)\@<!-\)\=\<0[xX]\x\+\%(_\x\+\)*\>"								display
syn match talonInteger	"\%(\%(\w\|[]})\"']\s*\)\@<!-\)\=\<\%(0[dD]\)\=\%(0\|[1-9]\d*\%(_\d\+\)*\)\>"						display
syn match talonInteger	"\%(\%(\w\|[]})\"']\s*\)\@<!-\)\=\<0[oO]\=\o\+\%(_\o\+\)*\>"								display
syn match talonInteger	"\%(\%(\w\|[]})\"']\s*\)\@<!-\)\=\<0[bB][01]\+\%(_[01]\+\)*\>"								display
syn match talonFloat	"\%(\%(\w\|[]})\"']\s*\)\@<!-\)\=\<\%(0\|[1-9]\d*\%(_\d\+\)*\)\.\d\+\%(_\d\+\)*\>"					display
syn match talonFloat	"\%(\%(\w\|[]})\"']\s*\)\@<!-\)\=\<\%(0\|[1-9]\d*\%(_\d\+\)*\)\%(\.\d\+\%(_\d\+\)*\)\=\%([eE][-+]\=\d\+\%(_\d\+\)*\)\>"	display

" Identifiers
syn match talonLocalVariableOrMethod "\<[_[:lower:]][_[:alnum:]]*[?!=]\=" contains=NONE display transparent

syn match  talonConstant		"\%(\%([.@$]\@<!\.\)\@<!\<\|::\)\_s*\zs\u\w*\%(\>\|::\)\@=\%(\s*(\)\@!"
syn match  talonInstanceVariable "@\h\w*"  display
syn match  talonGlobalVariable	"$\%(\h\w*\|-.\)"

syn match  talonAnnotation "%\w*"

syn match  talonBlockParameter	  "\h\w*" contained
syn region talonBlockParameterList start="\%(\%(\<do\>\|{\)\s*\)\@<=|" end="|" oneline display contains=talonBlockParameter

syn match talonInvalidVariable	 "$[^ A-Za-z_-]"

" Normal String and Shell Command Output
syn region talonString matchgroup=talonStringDelimiter start="\"" end="\"" skip="\\\\\|\\\"" contains=@talonStringSpecial fold
syn region talonString matchgroup=talonStringDelimiter start="'"	end="'"  skip="\\\\\|\\'"  contains=talonQuoteEscape    fold
syn region talonString matchgroup=talonStringDelimiter start="`"	end="`"  skip="\\\\\|\\`"  contains=@talonStringSpecial fold

syn match  talonAliasDeclaration    "[^[:space:];#.()]\+" contained contains=talonGlobalVariable nextgroup=talonAliasDeclaration2 skipwhite
syn match  talonAliasDeclaration2   "[^[:space:];#.()]\+" contained contains=talonGlobalVariable
syn match  talonMethodDeclaration   "[^[:space:];#(]\+"	 contained contains=talonConstant,talonBoolean,talonPseudoVariable,talonInstanceVariable,talonGlobalVariable
syn match  talonClassDeclaration    "[^[:space:];#<]\+"	 contained contains=talonConstant,talonOperator
syn match  talonTraitDeclaration   "[^[:space:];#<]\+"	 contained contains=talonConstant,talonOperator
syn match  talonFunction "\<[_[:alpha:]][_[:alnum:]]*[?!=]\=[[:alnum:]_.:?!=]\@!" contained containedin=talonMethodDeclaration
syn match  talonFunction "\%(\s\|^\)\@<=[_[:alpha:]][_[:alnum:]]*[?!=]\=\%(\s\|$\)\@=" contained containedin=talonAliasDeclaration,talonAliasDeclaration2
syn match  talonFunction "\%([[:space:].]\|^\)\@<=\%(\[\]=\=\|\*\*\|[+-]@\=\|[*/%|&^~]\|<<\|>>\|[<>]=\=\|<=>\|===\|==\|=\~\|`\)\%([[:space:];#(]\|$\)\@=" contained containedin=talonAliasDeclaration,talonAliasDeclaration2,talonMethodDeclaration

syn cluster talonDeclaration contains=talonAliasDeclaration,talonAliasDeclaration2,talonMethodDeclaration,talonTraitDeclaration,talonClassDeclaration,talonFunction,talonBlockParameter

" Keywords
" Note: the following keywords have already been defined:
" begin case class def do end for if trait unless until while
syn match   talonControl	       "\<\%(and\|break\|in\|next\|not\|or\|redo\|rescue\|retry\|return\)\>[?!]\@!"
syn match   talonKeyword	       "\<\%(super\|yield\)\>[?!]\@!"
syn match   talonBoolean	       "\<\%(true\|false\)\>[?!]\@!"
syn match   talonPseudoVariable "\<\%(nil\|self\|__ENCODING__\|__FILE__\|__LINE__\|__callee__\|__method__\)\>[?!]\@!" " TODO: reorganise
syn match   talonBeginEnd       "\<\%(BEGIN\|END\)\>[?!]\@!"

" Expensive Mode - match 'end' with the appropriate opening keyword for syntax
" based folding and special highlighting of trait/class/method definitions
if !exists("b:talon_no_expensive") && !exists("talon_no_expensive")
  syn match  talonDefine "\<alias\>"  nextgroup=talonAliasDeclaration  skipwhite skipnl
  syn match  talonDefine "\<def\>"    nextgroup=talonMethodDeclaration skipwhite skipnl
  syn match  talonDefine "\<dec\>"    nextgroup=talonMethodDeclaration skipwhite skipnl
  syn match  talonDefine "\<undef\>"  nextgroup=talonFunction	     skipwhite skipnl
  syn match  talonClass	"\<class\>"  nextgroup=talonClassDeclaration  skipwhite skipnl
  syn match  talonTrait "\<trait\>" nextgroup=talonTraitDeclaration skipwhite skipnl

  syn region talonMethodBlock start="\<def\>"	matchgroup=talonDefine end="\%(\<def\_s\+\)\@<!\<end\>" contains=ALLBUT,@talonNotTop fold
  syn region talonBlock	     start="\<class\>"	matchgroup=talonClass  end="\<end\>"		       contains=ALLBUT,@talonNotTop fold
  syn region talonBlock	     start="\<trait\>" matchgroup=talonTrait end="\<end\>"		       contains=ALLBUT,@talonNotTop fold

  " modifiers
  syn match talonConditionalModifier "\<\%(if\|unless\)\>"    display
  syn match talonRepeatModifier	     "\<\%(while\|until\)\>" display

  syn region talonDoBlock      matchgroup=talonControl start="\<do\>" end="\<end\>"                 contains=ALLBUT,@talonNotTop fold
  " curly bracket block or hash literal
  syn region talonCurlyBlock   start="{" end="}"							  contains=ALLBUT,@talonNotTop fold
  syn region talonArrayLiteral matchgroup=talonArrayDelimiter start="\%(\w\|[\]})]\)\@<!\[" end="]" contains=ALLBUT,@talonNotTop fold

  " statements without 'do'
  syn region talonBlockExpression       matchgroup=talonControl	  start="\<begin\>" end="\<end\>" contains=ALLBUT,@talonNotTop fold
  syn region talonCaseExpression	       matchgroup=talonConditional start="\<case\>"  end="\<end\>" contains=ALLBUT,@talonNotTop fold
  syn region talonConditionalExpression matchgroup=talonConditional start="\%(\%(^\|\.\.\.\=\|[{:,;([<>~\*/%&^|+=-]\|\%(\<[_[:lower:]][_[:alnum:]]*\)\@<![?!]\)\s*\)\@<=\%(if\|unless\)\>" end="\<end\>" contains=ALLBUT,@talonNotTop fold

  syn match talonConditional "\<\%(then\|else\|when\)\>[?!]\@!"	contained containedin=talonCaseExpression
  syn match talonConditional "\<\%(then\|else\|elsif\)\>[?!]\@!" contained containedin=talonConditionalExpression

  syn match talonExceptional	  "\<\%(\%(\%(;\|^\)\s*\)\@<=rescue\|else\|ensure\)\>[?!]\@!" contained containedin=talonBlockExpression
  syn match talonMethodExceptional "\<\%(\%(\%(;\|^\)\s*\)\@<=rescue\|else\|ensure\)\>[?!]\@!" contained containedin=talonMethodBlock

  " statements with optional 'do'
  syn region talonOptionalDoLine   matchgroup=talonRepeat start="\<for\>[?!]\@!" start="\%(\%(^\|\.\.\.\=\|[{:,;([<>~\*/%&^|+-]\|\%(\<[_[:lower:]][_[:alnum:]]*\)\@<![!=?]\)\s*\)\@<=\<\%(until\|while\)\>" matchgroup=talonOptionalDo end="\%(\<do\>\)" end="\ze\%(;\|$\)" oneline contains=ALLBUT,@talonNotTop
  syn region talonRepeatExpression start="\<for\>[?!]\@!" start="\%(\%(^\|\.\.\.\=\|[{:,;([<>~\*/%&^|+-]\|\%(\<[_[:lower:]][_[:alnum:]]*\)\@<![!=?]\)\s*\)\@<=\<\%(until\|while\)\>" matchgroup=talonRepeat end="\<end\>" contains=ALLBUT,@talonNotTop nextgroup=talonOptionalDoLine fold

  if !exists("talon_minlines")
    let talon_minlines = 50
  endif
  exec "syn sync minlines=" . talon_minlines

else
  syn match talonControl "\<def\>[?!]\@!"    nextgroup=talonMethodDeclaration skipwhite skipnl
  syn match talonControl "\<dec\>[?!]\@!"    nextgroup=talonMethodDeclaration skipwhite skipnl
  syn match talonControl "\<class\>[?!]\@!"  nextgroup=talonClassDeclaration  skipwhite skipnl
  syn match talonControl "\<trait\>[?!]\@!" nextgroup=talonTraitDeclaration skipwhite skipnl
  syn match talonControl "\<\%(case\|begin\|do\|for\|if\|unless\|while\|until\|else\|elsif\|ensure\|then\|when\|end\)\>[?!]\@!"
  syn match talonKeyword "\<\%(alias\|undef\)\>[?!]\@!"
endif

syn keyword talonAccess    public protected private public_class_method private_class_method module_function
" attr is a common variable name
syn match   talonAttribute "\%(\%(^\|;\)\s*\)\@<=attr\>\(\s*[.=]\)\@!"
syn keyword talonAttribute attr_accessor attr_reader attr_writer
syn match   talonControl   "\<\%(exit!\|\%(abort\|at_exit\|exit\|fork\|loop\|trap\)\>[?!]\@!\)"
syn keyword talonBuiltinTypes int char dynamic void
syn keyword talonException raise fail catch throw
" false positive with 'include?'
syn match   talonInclude   "\<include\>[?!]\@!"
syn keyword talonInclude   import package

syn keyword talonKeyword   var
syn match   talonKeyword   ":="

" Comments and Documentation
syn match   talonSharpBang "\%^#!.*" display
syn keyword talonTodo	  FIXME NOTE TODO OPTIMIZE XXX contained
syn match   talonComment   "--.*" contains=talonSharpBang,talonSpaceError,talonTodo,@Spell

" Note: this is a hack to prevent 'keywords' being highlighted as such when called as methods with an explicit receiver
syn match talonKeywordAsMethod "\%(\%(\.\@<!\.\)\|::\)\_s*\%(alias\|and\|begin\|break\|case\|class\|def\|defined\|do\|else\)\>"		  transparent contains=NONE
syn match talonKeywordAsMethod "\%(\%(\.\@<!\.\)\|::\)\_s*\%(elsif\|end\|ensure\|false\|for\|if\|trait\|next\|nil\)\>"		  transparent contains=NONE
syn match talonKeywordAsMethod "\%(\%(\.\@<!\.\)\|::\)\_s*\%(not\|or\|redo\|rescue\|retry\|return\|self\|super\|then\|true\)\>"		  transparent contains=NONE
syn match talonKeywordAsMethod "\%(\%(\.\@<!\.\)\|::\)\_s*\%(undef\|unless\|until\|when\|while\|yield\|BEGIN\|END\|__FILE__\|__LINE__\)\>" transparent contains=NONE

syn match talonKeywordAsMethod "\<\%(alias\|begin\|case\|class\|def\|do\|end\)[?!]" transparent contains=NONE
syn match talonKeywordAsMethod "\<\%(if\|trait\|undef\|unless\|until\|while\)[?!]" transparent contains=NONE

syn match talonKeywordAsMethod "\%(\%(\.\@<!\.\)\|::\)\_s*\%(abort\|at_exit\|attr\|attr_accessor\|attr_reader\)\>"   transparent contains=NONE
syn match talonKeywordAsMethod "\%(\%(\.\@<!\.\)\|::\)\_s*\%(attr_writer\|autoload\|callcc\|catch\|caller\)\>"	    transparent contains=NONE
syn match talonKeywordAsMethod "\%(\%(\.\@<!\.\)\|::\)\_s*\%(eval\|class_eval\|instance_eval\|trait_eval\|exit\)\>" transparent contains=NONE
syn match talonKeywordAsMethod "\%(\%(\.\@<!\.\)\|::\)\_s*\%(extend\|fail\|fork\|include\|lambda\)\>"		    transparent contains=NONE
syn match talonKeywordAsMethod "\%(\%(\.\@<!\.\)\|::\)\_s*\%(load\|loop\|private\|proc\|protected\)\>"		    transparent contains=NONE
syn match talonKeywordAsMethod "\%(\%(\.\@<!\.\)\|::\)\_s*\%(public\|require\|raise\|throw\|trap\)\>"		    transparent contains=NONE

" __END__ Directive
syn region talonData matchgroup=talonDataDirective start="^__END__$" end="\%$" fold

hi def link talonClass			talonDefine
hi def link talonTrait			talonDefine
hi def link talonMethodExceptional	talonDefine
hi def link talonDefine			Define
hi def link talonFunction		Function
hi def link talonAnnotation             Macro
hi def link talonConditional		Conditional
hi def link talonConditionalModifier	talonConditional
hi def link talonExceptional		talonConditional
hi def link talonRepeat			Repeat
hi def link talonRepeatModifier		talonRepeat
hi def link talonOptionalDo		talonRepeat
hi def link talonControl			Statement
hi def link talonInclude			Include
hi def link talonInteger			Number
hi def link talonASCIICode		Character
hi def link talonFloat			Float
hi def link talonBoolean			Boolean
hi def link talonException		Exception
if !exists("talon_no_identifiers")
  hi def link talonIdentifier		Identifier
else
  hi def link talonIdentifier		NONE
endif
hi def link talonConstant		Type
hi def link talonGlobalVariable		talonIdentifier
hi def link talonBlockParameter		talonIdentifier
hi def link talonInstanceVariable	talonIdentifier
hi def link talonKeyword			Keyword
hi def link talonOperator		Operator
hi def link talonBeginEnd		Statement
hi def link talonAccess			Statement
hi def link talonAttribute		Statement
hi def link talonBuiltinTypes		Type
hi def link talonPseudoVariable		Constant

hi def link talonComment			Comment
hi def link talonData			Comment
hi def link talonDataDirective		Delimiter
hi def link talonDocumentation		Comment
hi def link talonTodo			Todo

hi def link talonQuoteEscape		talonStringEscape
hi def link talonStringEscape		Special
hi def link talonInterpolationDelimiter	Delimiter
hi def link talonNoInterpolation		talonString
hi def link talonSharpBang		PreProc
hi def link talonStringDelimiter		Delimiter
hi def link talonString			String

hi def link talonInvalidVariable		Error
hi def link talonError			Error
hi def link talonSpaceError		talonError

let b:current_syntax = "talon"

" vim: nowrap sw=2 sts=2 ts=8 noet:
