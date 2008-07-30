{
module AnaPresParse where

import Char
import Control.Monad
import TransMaker
import Data.List
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

lexer :: String -> IO [Token]
lexer []        = return []
lexer ('{':cs)  = (TokOpen  :) `liftM` lexer cs
lexer ('}':cs)  = (TokClose :) `liftM` lexer cs
lexer ('"':cs)  = case span (/='"') cs of
		      (word,'"':cs') -> (TokWord word :) `liftM` lexer cs'
lexer cs     | "#include \"" `isPrefixOf` cs = do
			let (file, '"':cs') = span (/='"') (drop (length "#include \"") cs)
			input <- readFile file
			rest <- lexer cs'
			return $ (map TokWord . filter (all isUpper) . words $input ) ++ rest
lexer (c:cs) | isSpace c  = lexer cs
             | isAlpha c  = lexStr (c:cs)

lexStr cs = case span isAlpha cs of 
		("frame",r)	-> (TokFrame :) `liftM` lexer r
		("hidden",r)	-> (TokHidden :) `liftM` lexer r
		("twolines",r)	-> (TokTwoLines :) `liftM` lexer r
		("threelines",r)-> (TokThreeLines :) `liftM` lexer r
		("flip",r)	-> (TokFlip :) `liftM` lexer r
		("fadeout",r)   -> (TokFade :) `liftM` lexer r
		("nix",r)       -> (TokNix :) `liftM` lexer r
		(s,r)           -> if all isUpper s then (TokWord s :) `liftM` lexer r
		                                    else error $ "Unknown "++ s

happyError = error . unwords . map show

readAnaPres file = anapres_parse `liftM` (lexer =<< readFile file)

}
