if exists("b:current_syntax")
  finish
endif

if has("folding") && exists("marius_fold")
  setlocal foldmethod=syntax
endif

syn cluster mariusNotTop contains=@mariusExtendedStringSpecial,@mariusDeclaration,mariusConditional,mariusExceptional,mariusMethodExceptional,mariusTodo

if exists("marius_space_errors")
  if !exists("marius_no_trail_space_error")
    syn match mariusSpaceError display excludenl "\s\+$"
  endif
  if !exists("marius_no_tab_space_error")
    syn match mariusSpaceError display " \+\t"me=e-1
  endif
endif

" Operators
if exists("marius_operators")
  syn match  mariusOperator "[~!^&|*/%+-]\|\%(class\s*\)\@<!<<\|<=>\|<=\|\%(<\|\<class\s\+\u\w*\s*\)\@<!<[^<]\@=\|===\|==\|=\~\|>>\|>=\|=\@<!>\|\*\*\|\.\.\.\|\.\.\|::"
  syn match  mariusOperator "->\|-=\|/=\|\*\*=\|\*=\|&&=\|&=\|&&\|||=\||=\|||\|%=\|+=\|!\~\|!="
  syn region mariusBracketOperator matchgroup=mariusOperator start="\%(\w[?!]\=\|[]})]\)\@<=\[\s*" end="\s*]" contains=ALLBUT,@mariusNotTop
endif

" Expression Substitution and Backslash Notation
syn match mariusStringEscape "\\\\\|\\[abefnrstv]\|\\\o\{1,3}\|\\x\x\{1,2}"						    contained display
syn match mariusStringEscape "\%(\\M-\\C-\|\\C-\\M-\|\\M-\\c\|\\c\\M-\|\\c\|\\C-\|\\M-\)\%(\\\o\{1,3}\|\\x\x\{1,2}\|\\\=\S\)" contained display
syn match mariusQuoteEscape  "\\[\\']"											    contained display

syn region mariusInterpolation	      matchgroup=mariusInterpolationDelimiter start="#{" end="}" contained contains=ALLBUT,@mariusNotTop
syn match  mariusInterpolation	      "#\%(\$\|@@\=\)\w\+"    display contained contains=mariusInterpolationDelimiter,mariusInstanceVariable,mariusGlobalVariable
syn match  mariusInterpolationDelimiter "#\ze\%(\$\|@@\=\)\w\+" display contained
syn match  mariusInterpolation	      "#\$\%(-\w\|\W\)"       display contained contains=mariusInterpolationDelimiter,mariusInvalidVariable
syn match  mariusInterpolationDelimiter "#\ze\$\%(-\w\|\W\)"    display contained
syn region mariusNoInterpolation	      start="\\#{" end="}"            contained
syn match  mariusNoInterpolation	      "\\#{"		      display contained
syn match  mariusNoInterpolation	      "\\#\%(\$\|@@\=\)\w\+"  display contained
syn match  mariusNoInterpolation	      "\\#\$\W"		      display contained

syn match mariusDelimEscape	"\\[(<{\[)>}\]]" transparent display contained contains=NONE

syn region mariusNestedParentheses    start="("  skip="\\\\\|\\)"  matchgroup=mariusString end=")"	transparent contained
syn region mariusNestedCurlyBraces    start="{"  skip="\\\\\|\\}"  matchgroup=mariusString end="}"	transparent contained
syn region mariusNestedAngleBrackets  start="<"  skip="\\\\\|\\>"  matchgroup=mariusString end=">"	transparent contained
syn region mariusNestedSquareBrackets start="\[" skip="\\\\\|\\\]" matchgroup=mariusString end="\]"	transparent contained

syn cluster mariusStringSpecial	      contains=mariusInterpolation,mariusNoInterpolation,mariusStringEscape
syn cluster mariusExtendedStringSpecial contains=@mariusStringSpecial,mariusNestedParentheses,mariusNestedCurlyBraces,mariusNestedAngleBrackets,mariusNestedSquareBrackets

" Numbers and ASCII Codes
syn match mariusASCIICode	"\%(\w\|[]})\"'/]\)\@<!\%(?\%(\\M-\\C-\|\\C-\\M-\|\\M-\\c\|\\c\\M-\|\\c\|\\C-\|\\M-\)\=\%(\\\o\{1,3}\|\\x\x\{1,2}\|\\\=\S\)\)"
syn match mariusInteger	"\%(\%(\w\|[]})\"']\s*\)\@<!-\)\=\<0[xX]\x\+\%(_\x\+\)*\>"								display
syn match mariusInteger	"\%(\%(\w\|[]})\"']\s*\)\@<!-\)\=\<\%(0[dD]\)\=\%(0\|[1-9]\d*\%(_\d\+\)*\)\>"						display
syn match mariusInteger	"\%(\%(\w\|[]})\"']\s*\)\@<!-\)\=\<0[oO]\=\o\+\%(_\o\+\)*\>"								display
syn match mariusInteger	"\%(\%(\w\|[]})\"']\s*\)\@<!-\)\=\<0[bB][01]\+\%(_[01]\+\)*\>"								display
syn match mariusFloat	"\%(\%(\w\|[]})\"']\s*\)\@<!-\)\=\<\%(0\|[1-9]\d*\%(_\d\+\)*\)\.\d\+\%(_\d\+\)*\>"					display
syn match mariusFloat	"\%(\%(\w\|[]})\"']\s*\)\@<!-\)\=\<\%(0\|[1-9]\d*\%(_\d\+\)*\)\%(\.\d\+\%(_\d\+\)*\)\=\%([eE][-+]\=\d\+\%(_\d\+\)*\)\>"	display

" Identifiers
syn match mariusLocalVariableOrMethod "\<[_[:lower:]][_[:alnum:]]*[?!=]\=" contains=NONE display transparent

syn match  mariusConstant		"\%(\%([.@$]\@<!\.\)\@<!\<\|::\)\_s*\zs\u\w*\%(\>\|::\)\@=\%(\s*(\)\@!"
syn match  mariusInstanceVariable "@\h\w*"  display
syn match  mariusGlobalVariable	"$\%(\h\w*\|-.\)"

syn match  mariusAnnotation "%\w*"

syn match  mariusBlockParameter	  "\h\w*" contained
syn region mariusBlockParameterList start="\%(\%(\<do\>\|{\)\s*\)\@<=|" end="|" oneline display contains=mariusBlockParameter

syn match mariusInvalidVariable	 "$[^ A-Za-z_-]"

" Normal String and Shell Command Output
syn region mariusString matchgroup=mariusStringDelimiter start="\"" end="\"" skip="\\\\\|\\\"" contains=@mariusStringSpecial fold
syn region mariusString matchgroup=mariusStringDelimiter start="'"	end="'"  skip="\\\\\|\\'"  contains=mariusQuoteEscape    fold
syn region mariusString matchgroup=mariusStringDelimiter start="`"	end="`"  skip="\\\\\|\\`"  contains=@mariusStringSpecial fold

syn match  mariusAliasDeclaration    "[^[:space:];#.()]\+" contained contains=mariusGlobalVariable nextgroup=mariusAliasDeclaration2 skipwhite
syn match  mariusAliasDeclaration2   "[^[:space:];#.()]\+" contained contains=mariusGlobalVariable
syn match  mariusMethodDeclaration   "[^[:space:];#(]\+"	 contained contains=mariusConstant,mariusBoolean,mariusPseudoVariable,mariusInstanceVariable,mariusGlobalVariable
syn match  mariusClassDeclaration    "[^[:space:];#<]\+"	 contained contains=mariusConstant,mariusOperator
syn match  mariusTraitDeclaration   "[^[:space:];#<]\+"	 contained contains=mariusConstant,mariusOperator
syn match  mariusFunction "\<[_[:alpha:]][_[:alnum:]]*[?!=]\=[[:alnum:]_.:?!=]\@!" contained containedin=mariusMethodDeclaration
syn match  mariusFunction "\%(\s\|^\)\@<=[_[:alpha:]][_[:alnum:]]*[?!=]\=\%(\s\|$\)\@=" contained containedin=mariusAliasDeclaration,mariusAliasDeclaration2
syn match  mariusFunction "\%([[:space:].]\|^\)\@<=\%(\[\]=\=\|\*\*\|[+-]@\=\|[*/%|&^~]\|<<\|>>\|[<>]=\=\|<=>\|===\|==\|=\~\|`\)\%([[:space:];#(]\|$\)\@=" contained containedin=mariusAliasDeclaration,mariusAliasDeclaration2,mariusMethodDeclaration

syn cluster mariusDeclaration contains=mariusAliasDeclaration,mariusAliasDeclaration2,mariusMethodDeclaration,mariusTraitDeclaration,mariusClassDeclaration,mariusFunction,mariusBlockParameter

" Keywords
" Note: the following keywords have already been defined:
" begin case class def do end for if trait unless until while
syn match   mariusControl	       "\<\%(and\|break\|in\|next\|not\|or\|redo\|rescue\|retry\|return\)\>[?!]\@!"
syn match   mariusKeyword	       "\<\%(super\|yield\)\>[?!]\@!"
syn match   mariusBoolean	       "\<\%(true\|false\)\>[?!]\@!"
syn match   mariusPseudoVariable "\<\%(nil\|self\|__ENCODING__\|__FILE__\|__LINE__\|__callee__\|__method__\)\>[?!]\@!" " TODO: reorganise
syn match   mariusBeginEnd       "\<\%(BEGIN\|END\)\>[?!]\@!"

" Expensive Mode - match 'end' with the appropriate opening keyword for syntax
" based folding and special highlighting of trait/class/method definitions
if !exists("b:marius_no_expensive") && !exists("marius_no_expensive")
  syn match  mariusDefine "\<alias\>"  nextgroup=mariusAliasDeclaration  skipwhite skipnl
  syn match  mariusDefine "\<def\>"    nextgroup=mariusMethodDeclaration skipwhite skipnl
  syn match  mariusDefine "\<dec\>"    nextgroup=mariusMethodDeclaration skipwhite skipnl
  syn match  mariusDefine "\<undef\>"  nextgroup=mariusFunction	     skipwhite skipnl
  syn match  mariusClass	"\<class\>"  nextgroup=mariusClassDeclaration  skipwhite skipnl
  syn match  mariusTrait "\<trait\>" nextgroup=mariusTraitDeclaration skipwhite skipnl

  syn region mariusMethodBlock start="\<def\>"	matchgroup=mariusDefine end="\%(\<def\_s\+\)\@<!\<end\>" contains=ALLBUT,@mariusNotTop fold
  syn region mariusBlock	     start="\<class\>"	matchgroup=mariusClass  end="\<end\>"		       contains=ALLBUT,@mariusNotTop fold
  syn region mariusBlock	     start="\<trait\>" matchgroup=mariusTrait end="\<end\>"		       contains=ALLBUT,@mariusNotTop fold

  " modifiers
  syn match mariusConditionalModifier "\<\%(if\|unless\)\>"    display
  syn match mariusRepeatModifier	     "\<\%(while\|until\)\>" display

  syn region mariusDoBlock      matchgroup=mariusControl start="\<do\>" end="\<end\>"                 contains=ALLBUT,@mariusNotTop fold
  " curly bracket block or hash literal
  syn region mariusCurlyBlock   start="{" end="}"							  contains=ALLBUT,@mariusNotTop fold
  syn region mariusArrayLiteral matchgroup=mariusArrayDelimiter start="\%(\w\|[\]})]\)\@<!\[" end="]" contains=ALLBUT,@mariusNotTop fold

  " statements without 'do'
  syn region mariusBlockExpression       matchgroup=mariusControl	  start="\<try\>" end="\<end\>" contains=ALLBUT,@mariusNotTop fold
  syn region mariusCaseExpression	       matchgroup=mariusConditional start="\<case\>"  end="\<end\>" contains=ALLBUT,@mariusNotTop fold
  syn region mariusConditionalExpression matchgroup=mariusConditional start="\%(\%(^\|\.\.\.\=\|[{:,;([<>~\*/%&^|+=-]\|\%(\<[_[:lower:]][_[:alnum:]]*\)\@<![?!]\)\s*\)\@<=\%(if\|unless\)\>" end="\<end\>" contains=ALLBUT,@mariusNotTop fold

  syn match mariusConditional "\<\%(then\|else\|when\)\>[?!]\@!"	contained containedin=mariusCaseExpression
  syn match mariusConditional "\<\%(then\|else\|elsif\)\>[?!]\@!" contained containedin=mariusConditionalExpression

  syn match mariusExceptional	  "\<\%(\%(\%(;\|^\)\s*\)\@<=rescue\|else\|ensure\)\>[?!]\@!" contained containedin=mariusBlockExpression
  syn match mariusMethodExceptional "\<\%(\%(\%(;\|^\)\s*\)\@<=rescue\|else\|ensure\)\>[?!]\@!" contained containedin=mariusMethodBlock

  " statements with optional 'do'
  syn region mariusOptionalDoLine   matchgroup=mariusRepeat start="\<for\>[?!]\@!" start="\%(\%(^\|\.\.\.\=\|[{:,;([<>~\*/%&^|+-]\|\%(\<[_[:lower:]][_[:alnum:]]*\)\@<![!=?]\)\s*\)\@<=\<\%(until\|while\)\>" matchgroup=mariusOptionalDo end="\%(\<do\>\)" end="\ze\%(;\|$\)" oneline contains=ALLBUT,@mariusNotTop
  syn region mariusRepeatExpression start="\<for\>[?!]\@!" start="\%(\%(^\|\.\.\.\=\|[{:,;([<>~\*/%&^|+-]\|\%(\<[_[:lower:]][_[:alnum:]]*\)\@<![!=?]\)\s*\)\@<=\<\%(until\|while\)\>" matchgroup=mariusRepeat end="\<end\>" contains=ALLBUT,@mariusNotTop nextgroup=mariusOptionalDoLine fold

  if !exists("marius_minlines")
    let marius_minlines = 50
  endif
  exec "syn sync minlines=" . marius_minlines

else
  syn match mariusControl "\<def\>[?!]\@!"    nextgroup=mariusMethodDeclaration skipwhite skipnl
  syn match mariusControl "\<dec\>[?!]\@!"    nextgroup=mariusMethodDeclaration skipwhite skipnl
  syn match mariusControl "\<class\>[?!]\@!"  nextgroup=mariusClassDeclaration  skipwhite skipnl
  syn match mariusControl "\<trait\>[?!]\@!" nextgroup=mariusTraitDeclaration skipwhite skipnl
  syn match mariusControl "\<\%(case\|try\|do\|for\|if\|unless\|while\|until\|else\|elsif\|ensure\|then\|when\|end\)\>[?!]\@!"
  syn match mariusKeyword "\<\%(alias\|undef\)\>[?!]\@!"
endif

syn keyword mariusAccess    public protected private public_class_method private_class_method module_function
" attr is a common variable name
syn match   mariusAttribute "\%(\%(^\|;\)\s*\)\@<=attr\>\(\s*[.=]\)\@!"
syn keyword mariusAttribute attr_accessor attr_reader attr_writer
syn match   mariusControl   "\<\%(exit!\|\%(abort\|at_exit\|exit\|fork\|loop\|trap\)\>[?!]\@!\)"
syn keyword mariusBuiltinTypes int char dynamic void
syn keyword mariusException raise fail catch throw
" false positive with 'include?'
syn keyword mariusInclude   uses import as package

syn keyword mariusKeyword   var
syn match   mariusKeyword   ":="

" Comments and Documentation
syn match   mariusSharpBang "\%^#!.*" display
syn keyword mariusTodo	  FIXME NOTE TODO OPTIMIZE XXX contained
syn match   mariusComment   "#.*" contains=mariusSharpBang,mariusSpaceError,mariusTodo,@Spell

" Note: this is a hack to prevent 'keywords' being highlighted as such when called as methods with an explicit receiver
syn match mariusKeywordAsMethod "\%(\%(\.\@<!\.\)\|::\)\_s*\%(alias\|and\|try\|break\|case\|class\|def\|defined\|do\|else\)\>"		  transparent contains=NONE
syn match mariusKeywordAsMethod "\%(\%(\.\@<!\.\)\|::\)\_s*\%(elsif\|end\|ensure\|false\|for\|if\|trait\|next\|nil\)\>"		  transparent contains=NONE
syn match mariusKeywordAsMethod "\%(\%(\.\@<!\.\)\|::\)\_s*\%(not\|or\|redo\|rescue\|retry\|return\|self\|super\|then\|true\)\>"		  transparent contains=NONE
syn match mariusKeywordAsMethod "\%(\%(\.\@<!\.\)\|::\)\_s*\%(undef\|unless\|until\|when\|while\|yield\|BEGIN\|END\|__FILE__\|__LINE__\)\>" transparent contains=NONE

syn match mariusKeywordAsMethod "\<\%(alias\|try\|case\|class\|def\|do\|end\)[?!]" transparent contains=NONE
syn match mariusKeywordAsMethod "\<\%(if\|trait\|undef\|unless\|until\|while\)[?!]" transparent contains=NONE

syn match mariusKeywordAsMethod "\%(\%(\.\@<!\.\)\|::\)\_s*\%(abort\|at_exit\|attr\|attr_accessor\|attr_reader\)\>"   transparent contains=NONE
syn match mariusKeywordAsMethod "\%(\%(\.\@<!\.\)\|::\)\_s*\%(attr_writer\|autoload\|callcc\|catch\|caller\)\>"	    transparent contains=NONE
syn match mariusKeywordAsMethod "\%(\%(\.\@<!\.\)\|::\)\_s*\%(eval\|class_eval\|instance_eval\|trait_eval\|exit\)\>" transparent contains=NONE
syn match mariusKeywordAsMethod "\%(\%(\.\@<!\.\)\|::\)\_s*\%(extend\|fail\|fork\|include\|lambda\)\>"		    transparent contains=NONE
syn match mariusKeywordAsMethod "\%(\%(\.\@<!\.\)\|::\)\_s*\%(load\|loop\|private\|proc\|protected\)\>"		    transparent contains=NONE
syn match mariusKeywordAsMethod "\%(\%(\.\@<!\.\)\|::\)\_s*\%(public\|require\|raise\|throw\|trap\)\>"		    transparent contains=NONE

" __END__ Directive
syn region mariusData matchgroup=mariusDataDirective start="^__END__$" end="\%$" fold

hi def link mariusClass			mariusDefine
hi def link mariusTrait			mariusDefine
hi def link mariusMethodExceptional	mariusDefine
hi def link mariusDefine			Define
hi def link mariusFunction		Function
hi def link mariusAnnotation             Macro
hi def link mariusConditional		Conditional
hi def link mariusConditionalModifier	mariusConditional
hi def link mariusExceptional		mariusConditional
hi def link mariusRepeat			Repeat
hi def link mariusRepeatModifier		mariusRepeat
hi def link mariusOptionalDo		mariusRepeat
hi def link mariusControl			Statement
hi def link mariusInclude			Include
hi def link mariusInteger			Number
hi def link mariusASCIICode		Character
hi def link mariusFloat			Float
hi def link mariusBoolean			Boolean
hi def link mariusException		Exception
if !exists("marius_no_identifiers")
  hi def link mariusIdentifier		Identifier
else
  hi def link mariusIdentifier		NONE
endif
hi def link mariusConstant		Type
hi def link mariusGlobalVariable		mariusIdentifier
hi def link mariusBlockParameter		mariusIdentifier
hi def link mariusInstanceVariable	mariusIdentifier
hi def link mariusKeyword			Keyword
hi def link mariusOperator		Operator
hi def link mariusBeginEnd		Statement
hi def link mariusAccess			Statement
hi def link mariusAttribute		Statement
hi def link mariusBuiltinTypes		Type
hi def link mariusPseudoVariable		Constant

hi def link mariusComment			Comment
hi def link mariusData			Comment
hi def link mariusDataDirective		Delimiter
hi def link mariusDocumentation		Comment
hi def link mariusTodo			Todo

hi def link mariusQuoteEscape		mariusStringEscape
hi def link mariusStringEscape		Special
hi def link mariusInterpolationDelimiter	Delimiter
hi def link mariusNoInterpolation		mariusString
hi def link mariusSharpBang		PreProc
hi def link mariusStringDelimiter		Delimiter
hi def link mariusString			String

hi def link mariusInvalidVariable		Error
hi def link mariusError			Error
hi def link mariusSpaceError		mariusError

let b:current_syntax = "marius"

" vim: nowrap sw=2 sts=2 ts=8 noet:
