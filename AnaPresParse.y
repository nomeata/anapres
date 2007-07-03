{
module AnaPresParse where

import Char
import Control.Monad
import TransMaker
}


%name  anapres_parse
%tokentype { Token }

%token
	'{'	 { TokOpen }
	'}'	 { TokClose }
	frame    { TokFrame }
	hidden	 { TokHidden }
	twolines { TokTwoLines }
	threelines { TokThreeLines }
	flip	 { TokFlip }
	fade	 { TokFade }
	nix	 { TokNix }
	char	 { TokChar $$ }
	word	 { TokWord $$ }

%%

AnaPres : frame Word Word '{' TransList '}' { frame $2 $3 $5 }

TransList : TransList fade   { fadeTrans $1}
          | TransList flip Char Char { flipTrans $3 $4 $1 }
	  | TransList Layout    { stdTrans $2 $1 }
	  | Layout              { start $1 }

Layout	: Word		             	{ moveMiddle $1 }
	| twolines Word Word	        { moveTwoLines $2 $3 }
	| threelines Word Word Word	{ moveThreeLines $2 $3 $4 }
	| hidden                        { moveHidden }

Word : word                            { $1 }
     | nix                             { "" }

Char : char                            { $1 }

{

data Token =	  TokOpen
		| TokClose
		| TokFrame
		| TokHidden
		| TokTwoLines
		| TokThreeLines
		| TokFlip
		| TokFade
		| TokNix
		| TokChar Char
		| TokWord String
	deriving Show

lexer :: String -> [Token]
lexer []        = []
lexer ('{':cs)  = TokOpen  : lexer cs
lexer ('}':cs)  = TokClose : lexer cs
lexer ('"':cs)  = case span (/='"') cs of
		      (word,'"':cs') -> TokWord word : lexer cs'
lexer (c:cs) | isSpace c  = lexer cs
             | isAlpha c  = lexStr (c:cs)

lexStr cs = case span isAlpha cs of 
		("frame",r)	-> TokFrame : lexer r
		("hidden",r)	-> TokHidden : lexer r
		("twolines",r)	-> TokTwoLines : lexer r
		("threelines",r)-> TokThreeLines : lexer r
		("flip",r)	-> TokFlip : lexer r
		("fadeout",r)   -> TokFade : lexer r
		("nix",r)       -> TokNix : lexer r
		(s,r)           -> if all isUpper s then TokWord s : lexer r
		                                    else error $ "Unknown "++ s

happyError = error . unwords . map show

readAnaPres file = (anapres_parse . lexer ) `liftM` readFile file

}
