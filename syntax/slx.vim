" Vim syntax file
" Language:	slx
" Maintainer:   Martin Schneider
" Last Change:  13.5.2012

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn match       cName		"\<[a-zA-Z_][a-zA-Z_0-9]*\>"
syn match	cFunction	"\<[a-zA-Z_][a-zA-Z_0-9]*\>[^()]*)("me=e-2
syn match	cFunction	"\<[a-zA-Z_][a-zA-Z_0-9]*\>\s*("me=e-1
syn match	cBraces		"[{}]"

syn keyword slxFunction	MULU_ DIVU_ MODU_ MUL_ DIV_ MOD_
syn keyword slxFunction	main typeof
syn keyword slxFunction	open close read write lseek dup dup2
syn keyword slxFunction	fcntl ioctl
syn keyword slxFunction	wctrans towctrans towupper
syn keyword slxFunction	towlower wctype iswctype
syn keyword slxFunction	iswxdigit iswupper iswspace
syn keyword slxFunction	iswpunct iswprint iswlower
syn keyword slxFunction	iswgraph iswdigit iswcntrl
syn keyword slxFunction	iswalpha iswalnum wcsrtombs
syn keyword slxFunction	mbsrtowcs wcrtomb mbrtowc
syn keyword slxFunction	mbrlen mbsinit wctob
syn keyword slxFunction	btowc wcsfxtime wcsftime
syn keyword slxFunction	wmemset wmemmove wmemcpy
syn keyword slxFunction	wmemcmp wmemchr wcstok
syn keyword slxFunction	wcsstr wcsspn wcsrchr
syn keyword slxFunction	wcspbrk wcslen wcscspn
syn keyword slxFunction	wcschr wcsxfrm wcsncmp
syn keyword slxFunction	wcscoll wcscmp wcsncat
syn keyword slxFunction	wcscat wcsncpy wcscpy
syn keyword slxFunction	wcstoull wcstoul wcstoll
syn keyword slxFunction	wcstol wcstold wcstof
syn keyword slxFunction	wcstod ungetwc putwchar
syn keyword slxFunction	putwc getwchar getwc
syn keyword slxFunction	fwide fputws fputwc
syn keyword slxFunction	fgetws fgetwc wscanf
syn keyword slxFunction	wprintf vwscanf vwprintf
syn keyword slxFunction	vswscanf vswprintf vfwscanf
syn keyword slxFunction	vfwprintf swscanf swprintf
syn keyword slxFunction	fwscanf fwprintf zonetime
syn keyword slxFunction	strfxtime strftime localtime
syn keyword slxFunction	gmtime ctime asctime
syn keyword slxFunction	time mkxtime mktime
syn keyword slxFunction	difftime clock strlen
syn keyword slxFunction	strerror memset strtok
syn keyword slxFunction	strstr strspn strrchr
syn keyword slxFunction	strpbrk strcspn strchr
syn keyword slxFunction	memchr strxfrm strncmp
syn keyword slxFunction	strcoll strcmp memcmp
syn keyword slxFunction	strncat strcat strncpy
syn keyword slxFunction	strcpy memmove memcpy
syn keyword slxFunction	wcstombs mbstowcs wctomb
syn keyword slxFunction	mbtowc mblen lldiv
syn keyword slxFunction	ldiv div llabs
syn keyword slxFunction	labs abs qsort
syn keyword slxFunction	bsearch system getenv
syn keyword slxFunction	exit atexit abort
syn keyword slxFunction	realloc malloc free
syn keyword slxFunction	calloc srand rand
syn keyword slxFunction	strtoull strtoul strtoll
syn keyword slxFunction	strtol strtold strtof
syn keyword slxFunction	strtod atoll atol
syn keyword slxFunction	atoi atof perror
syn keyword slxFunction	ferror feof clearerr
syn keyword slxFunction	rewind ftell fsetpos
syn keyword slxFunction	fseek fgetpos fwrite
syn keyword slxFunction	fread ungetc puts
syn keyword slxFunction	putchar putc gets
syn keyword slxFunction	getchar getc fputs
syn keyword slxFunction	fputc fgets fgetc
syn keyword slxFunction	vsscanf vsprintf vsnprintf
syn keyword slxFunction	vscanf vprintf vfscanf
syn keyword slxFunction	vfprintf sscanf sprintf
syn keyword slxFunction	snprintf scanf printf
syn keyword slxFunction	fscanf fprintf setvbuf
syn keyword slxFunction	setbuf freopen fopen
syn keyword slxFunction	fflush fclose tmpnam
syn keyword slxFunction	tmpfile rename remove
syn keyword slxFunction	offsetof va_start va_end
syn keyword slxFunction	va_copy va_arg raise signal
syn keyword slxFunction	longjmp setjmp isunordered
syn keyword slxFunction	islessgreater islessequal isless
syn keyword slxFunction	isgreaterequal isgreater fmal
syn keyword slxFunction	fmaf fma fminl
syn keyword slxFunction	fminf fmin fmaxl
syn keyword slxFunction	fmaxf fmax fdiml
syn keyword slxFunction	fdimf fdim nextafterxl
syn keyword slxFunction	nextafterxf nextafterx nextafterl
syn keyword slxFunction	nextafterf nextafter nanl
syn keyword slxFunction	nanf nan copysignl
syn keyword slxFunction	copysignf copysign remquol
syn keyword slxFunction	remquof remquo remainderl
syn keyword slxFunction	remainderf remainder fmodl
syn keyword slxFunction	fmodf fmod truncl
syn keyword slxFunction	truncf trunc llroundl
syn keyword slxFunction	llroundf llround lroundl
syn keyword slxFunction	lroundf lround roundl
syn keyword slxFunction	roundf round llrintl
syn keyword slxFunction	llrintf llrint lrintl
syn keyword slxFunction	lrintf lrint rintl
syn keyword slxFunction	rintf rint nearbyintl
syn keyword slxFunction	nearbyintf nearbyint floorl
syn keyword slxFunction	floorf floor ceill
syn keyword slxFunction	ceilf ceil tgammal
syn keyword slxFunction	tgammaf tgamma lgammal
syn keyword slxFunction	lgammaf lgamma erfcl
syn keyword slxFunction	erfcf erfc erfl
syn keyword slxFunction	erff erf sqrtl
syn keyword slxFunction	sqrtf sqrt powl
syn keyword slxFunction	powf pow hypotl
syn keyword slxFunction	hypotf hypot fabsl
syn keyword slxFunction	fabsf fabs cbrtl
syn keyword slxFunction	cbrtf cbrt scalblnl
syn keyword slxFunction	scalblnf scalbln scalbnl
syn keyword slxFunction	scalbnf scalbn modfl
syn keyword slxFunction	modff modf logbl
syn keyword slxFunction	logbf logb log2l
syn keyword slxFunction	log2f log2 log1pl
syn keyword slxFunction	log1pf log1p log10l
syn keyword slxFunction	log10f log10 logl
syn keyword slxFunction	logf log ldexpl
syn keyword slxFunction	ldexpf ldexp ilogbl
syn keyword slxFunction	ilogbf ilogb frexpl
syn keyword slxFunction	frexpf frexp expm1l
syn keyword slxFunction	expm1f expm1 exp2l
syn keyword slxFunction	exp2f exp2 expl
syn keyword slxFunction	expf exp tanhl
syn keyword slxFunction	tanhf tanh sinhl
syn keyword slxFunction	sinhf sinh coshl
syn keyword slxFunction	coshf cosh atanhl
syn keyword slxFunction	atanhf atanh asinhl
syn keyword slxFunction	asinhf asinh acoshl
syn keyword slxFunction	acoshf acosh tanl
syn keyword slxFunction	tanf tan sinl
syn keyword slxFunction	sinf sin cosl
syn keyword slxFunction	cosf cos atan2l
syn keyword slxFunction	atan2f atan2 atanl
syn keyword slxFunction	atanf atan asinl
syn keyword slxFunction	asinf asin acosl
syn keyword slxFunction	acosf acos signbit
syn keyword slxFunction	isnormal isnan isinf
syn keyword slxFunction	isfinite fpclassify localeconv
syn keyword slxFunction	setlocale wcstoumax wcstoimax
syn keyword slxFunction	strtoumax strtoimax feupdateenv
syn keyword slxFunction	fesetenv feholdexcept fegetenv
syn keyword slxFunction	fesetround fegetround fetestexcept
syn keyword slxFunction	fesetexceptflag feraiseexcept fegetexceptflag
syn keyword slxFunction	feclearexcept toupper tolower
syn keyword slxFunction	isxdigit isupper isspace
syn keyword slxFunction	ispunct isprint islower
syn keyword slxFunction	isgraph isdigit iscntrl
syn keyword slxFunction	isalpha isalnum creall
syn keyword slxFunction	crealf creal cprojl
syn keyword slxFunction	cprojf cproj conjl
syn keyword slxFunction	conjf conj cimagl
syn keyword slxFunction	cimagf cimag cargl
syn keyword slxFunction	cargf carg csqrtl
syn keyword slxFunction	csqrtf csqrt cpowl
syn keyword slxFunction	cpowf cpow cabsl
syn keyword slxFunction	cabsf cabs clogl
syn keyword slxFunction	clogf clog cexpl
syn keyword slxFunction	cexpf cexp ctanhl
syn keyword slxFunction	ctanhf ctanh csinhl
syn keyword slxFunction	csinhf csinh ccoshl
syn keyword slxFunction	ccoshf ccosh catanhl
syn keyword slxFunction	catanhf catanh casinhl
syn keyword slxFunction	casinhf casinh cacoshl
syn keyword slxFunction	cacoshf cacosh ctanl
syn keyword slxFunction	ctanf ctan csinl
syn keyword slxFunction	csinf csin ccosl
syn keyword slxFunction	ccosf ccos catanl
syn keyword slxFunction	catanf catan casinl
syn keyword slxFunction	casinf casin cacosl
syn keyword slxFunction	cacosf cacos assert
syn keyword slxFunction	UINTMAX_C INTMAX_C UINT64_C
syn keyword slxFunction	UINT32_C UINT16_C UINT8_C
syn keyword slxFunction	INT64_C INT32_C INT16_C INT8_C


syn keyword	cAnsiName	errno environ
syn keyword	cAnsiName	stdout stdin stderr
syn keyword	cAnsiName	and bitor not_eq xor
syn keyword	cAnsiName	and_eq compl or xor_eq
syn keyword	cAnsiName	bitand not or_eq

" A bunch of useful C keywords
"syn keyword	cStatement	goto
syn keyword	cStatement	break return continue asm
syn keyword	cStatement      advance destroy fork interrupt interrupted parent pause reactivate reschedule resume wait until terminate

syn keyword	cLabel		default
syn keyword	cLabel		case
syn keyword	cConditional	if else switch
syn keyword	cRepeat		while for do

syn keyword	cTodo		contained TODO FIXME XXX
syn match	cTodo		contained "///[A-Z]!*"

" cCommentGroup allows adding matches for special things in comments
syn cluster	cCommentGroup	contains=cTodo

" String and Character constants
" Highlight special characters (those which have a backslash) differently
syn match	cSpecial	display contained "\\\(x\x\+\|\o\{1,3}\|.\|$\)"
if !exists("c_no_utf")
  syn match	cSpecial	display contained "\\\(u\x\{4}\|U\x\{8}\)"
endif
if exists("c_no_cformat")
  syn region	cString		start=+L\="+ skip=+\\\\\|\\"+ end=+"+ contains=cSpecial,@Spell
  " cCppString: same as cString, but ends at end of line
  syn region	cCppString	start=+L\="+ skip=+\\\\\|\\"\|\\$+ excludenl end=+"+ end='$' contains=cSpecial,@Spell
else
  syn match	cFormat		display "%\(\d\+\$\)\=[-+' #0*,]*\(\d*\|\*\|\*\d\+\$\)\(\.\(\d*\|\*\|\*\d\+\$\)\)\=\([hlLjztF]\|ll\|hh\)\=\([bdiuoxXDOUfeEgGcCsSpnAaK]\|\[\^\=.[^]]*\]\)" contained
  syn match	cFormat		display "%%" contained
  syn region	cString		start=+L\="+ skip=+\\\\\|\\"+ end=+"+ contains=cSpecial,cFormat,@Spell
  " cCppString: same as cString, but ends at end of line
  syn region	cCppString	start=+L\="+ skip=+\\\\\|\\"\|\\$+ excludenl end=+"+ end='$' contains=cSpecial,cFormat
  hi link cFormat cSpecial
endif
hi link cCppString cString

syn match	cCharacter	"L\='[^\\]'"
syn match	cCharacter	"L'[^']*'" contains=cSpecial
if exists("c_gnu")
  syn match	cSpecialError	"L\='\\[^'\"?\\abefnrtv]'"
  syn match	cSpecialCharacter "L\='\\['\"?\\abefnrtv]'"
else
  syn match	cSpecialError	"L\='\\[^'\"?\\abfnrtv]'"
  syn match	cSpecialCharacter "L\='\\['\"?\\abfnrtv]'"
endif
syn match	cSpecialCharacter display "L\='\\\o\{1,3}'"
syn match	cSpecialCharacter display "'\\x\x\{1,2}'"
syn match	cSpecialCharacter display "L'\\x\x\+'"

"when wanted, highlight trailing white space
if exists("c_space_errors")
  if !exists("c_no_trail_space_error")
    syn match	cSpaceError	display excludenl "\s\+$"
  endif
  if !exists("c_no_tab_space_error")
    syn match	cSpaceError	display " \+\t"me=e-1
  endif
endif

"catch errors caused by wrong parenthesis and brackets
" also accept <% for {, %> for }, <: for [ and :> for ] (C99)
syn cluster	cParenGroup	contains=cParenError,cIncluded,cSpecial,cCommentSkip,cCommentString,cComment2String,@cCommentGroup,cCommentStartError,cUserCont,cUserLabel,cUserLabel2,cGotoLabel,cBitField,cCommentSkip,cOctalZero,cCppOut,cCppOut2,cCppSkip,cFormat,cNumber,cFloat,cOctal,cOctalError,cNumbersCom
if exists("c_no_bracket_error")
  syn region	cParen		transparent start='(' end=')' contains=ALLBUT,@cParenGroup,cCppParen,cCppString,@Spell
  " cCppParen: same as cParen but ends at end-of-line; used in cDefine
  syn region	cCppParen	transparent start='(' skip='\\$' excludenl end=')' end='$' contained contains=ALLBUT,@cParenGroup,cParen,cString,@Spell
  syn match	cParenError	display ")"
" syn match	cErrInParen	display contained "[{}]\|<%\|%>"
  syn match	cErrInParen	display contained "[]\|<%\|%>"
else
  syn region	cParen		transparent start='(' end=')' contains=ALLBUT,@cParenGroup,cCppParen,cErrInBracket,cCppBracket,cCppString,@Spell
  " cCppParen: same as cParen but ends at end-of-line; used in cDefine
  syn region	cCppParen	transparent start='(' skip='\\$' excludenl end=')' end='$' contained contains=ALLBUT,@cParenGroup,cErrInBracket,cParen,cBracket,cString,@Spell
  syn match	cParenError	display "[\])]"
" syn match	cErrInParen	display contained "[\]{}]\|<%\|%>"
  syn match	cErrInParen	display contained "[\]]\|<%\|%>"
  syn region	cBracket	transparent start='\[\|<::\@!' end=']\|:>' contains=ALLBUT,@cParenGroup,cErrInParen,cCppParen,cCppBracket,cCppString,@Spell
  " cCppBracket: same as cParen but ends at end-of-line; used in cDefine
  syn region	cCppBracket	transparent start='\[\|<::\@!' skip='\\$' excludenl end=']\|:>' end='$' contained contains=ALLBUT,@cParenGroup,cErrInParen,cParen,cBracket,cString,@Spell
  syn match	cErrInBracket	display contained "[);{}]\|<%\|%>"
  "syn region	cBlock		transparent matchgroup=cBraces start='{' end='}' contains=ALLBUT,@cParenGroup,cCppParen,cCppBracket,cCppString,cBraceError,cErrInBracket
  "syn match	cBraceError	"}"
endif

"integer number, or floating point number without a dot and with "f".
syn case ignore
syn match	cNumbers	display transparent "\<\d\|\.\d" contains=cNumber,cFloat,cOctalError,cOctal
" Same, but without octal error (for comments)
syn match	cNumbersCom	display contained transparent "\<\d\|\.\d" contains=cNumber,cFloat,cOctal
syn match	cNumber		display contained "\d\+\(u\=l\{0,2}\|ll\=u\)\>"
"hex number
syn match	cNumber		display contained "0x\x\+\(u\=l\{0,2}\|ll\=u\)\>"
" Flag the first zero of an octal number as something special
syn match	cOctal		display contained "0\o\+\(u\=l\{0,2}\|ll\=u\)\>" contains=cOctalZero
syn match	cOctalZero	display contained "\<0"
syn match	cFloat		display contained "\d\+f"
"floating point number, with dot, optional exponent
syn match	cFloat		display contained "\d\+\.\d*\(e[-+]\=\d\+\)\=[fl]\="
"floating point number, starting with a dot, optional exponent
syn match	cFloat		display contained "\.\d\+\(e[-+]\=\d\+\)\=[fl]\=\>"
"floating point number, without dot, with exponent
syn match	cFloat		display contained "\d\+e[-+]\=\d\+[fl]\=\>"
if !exists("c_no_c99")
  "hexadecimal floating point number, optional leading digits, with dot, with exponent
  syn match	cFloat		display contained "0x\x*\.\x\+p[-+]\=\d\+[fl]\=\>"
  "hexadecimal floating point number, with leading digits, optional dot, with exponent
  syn match	cFloat		display contained "0x\x\+\.\=p[-+]\=\d\+[fl]\=\>"
endif

" flag an octal number with wrong digits
syn match	cOctalError	display contained "0\o*[89]\d*"
syn case match

if exists("xxxc_comment_strings")
  " A comment can contain cString, cCharacter and cNumber.
  " But a "*/" inside a cString in a cComment DOES end the comment!  So we
  " need to use a special type of cString: cCommentString, which also ends on
  " "*/", and sees a "*" at the start of the line as comment again.
  " Unfortunately this doesn't very well work for // type of comments :-(
  syntax match	cCommentSkip	contained "^\s*\*\($\|\s\+\)"
  syntax region cCommentString	contained start=+L\=\\\@<!"+ skip=+\\\\\|\\"+ end=+"+ end=+\*/+me=s-1 contains=cSpecial,cCommentSkip
  syntax region cComment2String	contained start=+L\=\\\@<!"+ skip=+\\\\\|\\"+ end=+"+ end="$" contains=cSpecial
  syntax region  cCommentL	start="//" skip="\\$" end="$" keepend contains=@cCommentGroup,cComment2String,cCharacter,cNumbersCom,cSpaceError,@Spell
  syntax region cComment	matchgroup=cCommentStart start="/\*" end="\*/" contains=@cCommentGroup,cCommentStartError,cCommentString,cCharacter,cNumbersCom,cSpaceError,@Spell
else
  syn region	cCommentL	start="//" skip="\\$" end="$" keepend contains=@cCommentGroup,cSpaceError,@Spell
  syn region	cComment	matchgroup=cCommentStart start="/\*" end="\*/" contains=@cCommentGroup,cCommentStartError,cSpaceError,@Spell
endif
" keep a // comment separately, it terminates a preproc. conditional
syntax match	cCommentError	display "\*/"
syntax match	cCommentStartError display "/\*"me=e-1 contained

syn keyword	cOperator	sizeof
syn keyword	slxType		int long short char void
syn keyword	slxType		signed unsigned float double

syn keyword	cStructure	struct union enum typedef procedure class module
syn keyword	cStorageClass	static register auto volatile extern const VOL
if exists("c_gnu")
  syn keyword	cStorageClass	inline __attribute__
endif
if !exists("c_no_c99")
  syn keyword	cStorageClass	inline restrict
endif

" Accept %: for # (C99)
syn region	cPreCondit	start="^\s*\(%:\|#\)\s*\(ifdef\|ifndef\)\>" skip="\\$" end="$" end="//"me=s-1 contains=cComment,cCppString,cCharacter,cCppParen,cParenError,cNumbers,cCommentError,cSpaceError
syn keyword	cDefined	defined contained
"syn match	cDefined	display contained "\<defined(\w\+)" contains=cName,cAnsiName
syn region	cPreConditIf	start="^\s*\(%:\|#\)\s*\(if\|elif\)\>" skip="\\$" end="$" end="//"me=s-1 contains=cDefined,cComment,cCppString,cCharacter,cCppParen,cParenError,cNumbers,cCommentError,cSpaceError
syn match	cPreCondit	display "^\s*\(%:\|#\)\s*\(else\|endif\)\>"
if !exists("c_no_if0")
  syn region	cCppOut		start="^\s*\(%:\|#\)\s*if\s\+0\+\>" end=".\@=\|$" contains=cCppOut2
  syn region	cCppOut2	contained start="0" end="^\s*\(%:\|#\)\s*\(endif\>\|else\>\|elif\>\)" contains=cSpaceError,cCppSkip
  syn region	cCppSkip	contained start="^\s*\(%:\|#\)\s*\(if\>\|ifdef\>\|ifndef\>\)" skip="\\$" end="^\s*\(%:\|#\)\s*endif\>" contains=cSpaceError,cCppSkip
endif
syn region	cIncluded	display contained start=+"+ skip=+\\\\\|\\"+ end=+"+
syn match	cIncluded	display contained "<[^>]*>"
syn match	cInclude	display "^import\>\s*["<]" contains=cIncluded
syn match cLineSkip	"\\$"
syn cluster	cPreProcGroup	contains=cPreConditIf,cPreCondit,cIncluded,cInclude,cDefined,cDefine,cErrInParen,cErrInBracket,cUserLabel,cUserLabel2,cGotoLabel,cSpecial,cOctalZero,cCppOut,cCppOut2,cCppSkip,cFormat,cNumber,cFloat,cOctal,cOctalError,cNumbersCom,cString,cCommentSkip,cCommentString,cComment2String,@cCommentGroup,cCommentStartError,cParen,cBracket,cMulti
"syn region	cDefine		start="^\s*\(%:\|#\)\s*\(define\|undef\)\>" skip="\\$" end="$" end="//"me=s-1 contains=ALLBUT,@cPreProcGroup,@Spell
syn region	cDefine		start="^\s*\(%:\|#\)\s*\(define\|undef\)\>" skip="\\$" end="$" end="//"me=s-1 contains=ALLBUT,@cPreProcGroup,cName,cFunction,slxFunction,@Spell
syn region	cPreProc	start="^\s*\(%:\|#\)\s*\(pragma\>\|line\>\|warning\>\|warn\>\|error\>\)" skip="\\$" end="$" keepend contains=ALLBUT,@cPreProcGroup,@Spell

" Highlight User Labels
syn cluster	cMultiGroup	contains=cIncluded,cSpecial,cCommentSkip,cCommentString,cComment2String,@cCommentGroup,cCommentStartError,cUserCont,cUserLabel,cUserLabel2,cGotoLabel,cBitField,cOctalZero,cCppOut,cCppOut2,cCppSkip,cFormat,cNumber,cFloat,cOctal,cOctalError,cNumbersCom,cCppParen,cCppBracket,cCppString
syn region	cMulti		transparent start='?' skip='::' end=':' contains=ALLBUT,@cMultiGroup,@Spell
" Avoid matching foo::bar() in C++ by requiring that the next char is not ':'
syn cluster	cLabelGroup	contains=cUserLabel
syn match	cUserCont	display "^\s*\I\i*\s*:$" contains=@cLabelGroup
syn match	cUserCont	display ";\s*\I\i*\s*:$" contains=@cLabelGroup
syn match	cUserCont	display "^\s*\I\i*\s*:[^:]"me=e-1 contains=@cLabelGroup
syn match	cUserCont	display ";\s*\I\i*\s*:[^:]"me=e-1 contains=@cLabelGroup

syn match	cUserLabel	display "\I\i*" contained
syn match	cUserLabel2	display "\I\i*:;\+"me=e-2
syn match	cGotoLabel	display "\<goto\s\+\I\i*;"me=e-1,hs=s+5 contains=cGoto
syn keyword	cGoto		contained goto

" Avoid recognizing most bitfields as labels
syn match	cBitField	display "^\s*\I\i*\s*:\s*[1-9]"me=e-1
syn match	cBitField	display ";\s*\I\i*\s*:\s*[1-9]"me=e-1

syn keyword	slxType	        int float double boolean string set
if exists("c_minlines")
  let b:c_minlines = c_minlines
else
  if !exists("c_no_if0")
    let b:c_minlines = 50	" #if 0 constructs can be long
  else
    let b:c_minlines = 15	" mostly for () constructs
  endif
endif
exec "syn sync ccomment cComment minlines=" . b:c_minlines

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_c_syn_inits")
  if version < 508
    let did_c_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink cFormat	cSpecial
  HiLink cCppString	cString
  HiLink cCommentL	cComment
  HiLink cCommentStart	cComment
  HiLink cLabel		Label
" HiLink cUserLabel	Label
  HiLink cUserLabel	UserLabel2
  HiLink cUserLabel2	UserLabel2
  HiLink cGotoLabel	UserLabel2
  HiLink cGoto		Statement
  HiLink cConditional	Conditional
  HiLink cRepeat	Repeat
  HiLink cCharacter	Character
  HiLink cSpecialCharacter cSpecial
  HiLink cNumber	Number
  HiLink cOctal		Number
  HiLink cOctalZero	PreProc		" link this to Error if you want
  HiLink cFloat		Float
  HiLink cOctalError	cError
  HiLink cParenError	cError
  HiLink cErrInParen	cError
  HiLink cErrInBracket	cError
  HiLink cCommentError	cError
  HiLink cCommentStartError	cError
  HiLink cSpaceError	cError
  HiLink cSpecialError	cError
  HiLink cOperator	Operator
  HiLink cOperatorBold	OperatorBold
  HiLink cStructure	Structure
  HiLink cStorageClass	StorageClass
  HiLink cInclude	Include
  HiLink cPreProc	PreProc
  HiLink cDefine	Macro
  HiLink cDefined	PreCondit
  HiLink cIncluded	cString
  HiLink cError		Error
  HiLink cStatement	Statement
  HiLink cPreCondit	PreCondit
  HiLink cPreConditIf	PreCondit
  HiLink slxType		Type
  HiLink cConstant	Constant
  HiLink cCommentString cString
  HiLink cComment2String cString
  HiLink cCommentSkip	cComment
  HiLink cString	String
  HiLink cComment	Comment
  HiLink cSpecial	SpecialChar
  HiLink cTodo		Todo
  HiLink cCppSkip	cCppOut
  HiLink cCppOut2	cCppOut
  HiLink cCppOut	Comment
  HiLink cMulti		Operator
  HiLink cMultiMG	Operator
  HiLink cFunction	Function
  HiLink slxFunction	StdFunction
  HiLink cName		Name
  HiLink cBitField	Name
  HiLink cAnsiName	StdName
  "HiLink cBlock	BlockBraces
  HiLink cBraces	BlockBraces
  "HiLink cBraceError	Error
  HiLink cMC		MicroController
  HiLink cAnsiFuncPtr	AnsiFuncPtr

  hi Function		gui=NONE guifg=#e86f00
  "hi StdFunction	gui=bold guifg=#ee0040
  hi StdFunction	gui=bold guifg=#e86f00
  hi Statement		gui=bold guifg=#a06129
  hi UserLabel2		gui=bold guifg=#c96129
  hi Operator		gui=NONE guifg=#000000
  hi OperatorBold	gui=bold guifg=#000000
  hi StdName		gui=bold guifg=#5276e6
  hi Name		gui=NONE guifg=#5276e6
  hi BlockBraces	gui=bold guifg=#000000
  hi Special		gui=NONE guifg=#a000a0
  hi Comment		gui=NONE guifg=grey62
  hi MicroController	gui=bold guifg=#d00000
  hi AnsiFuncPtr	gui=NONE guifg=#ff0000
" hi PreProc        	gui=NONE guifg=#6a5acd
  hi PreCondit      	gui=NONE guifg=#6a5acd
" hi Macro          	gui=NONE guifg=#0000ff

  delcommand HiLink
endif
hi Normal		gui=NONE guifg=#000000 guibg=Ivory1

let b:current_syntax = "slx"

" vim: ts=8
