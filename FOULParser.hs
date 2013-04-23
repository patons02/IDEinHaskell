module FOULParser where

import Data.Char
import Data.List
import Control.Applicative
import Data.Traversable
import FOUL

{----
(0) IDENTIFY YOURSELF

Name: Stuart Paton

----}

newtype Parser x =
  Parser {parse :: String -> Either (String, String) (x, String)} --apply function to relevant input then do case analysis on result

{----
(1) Define suitable instances of Applicative and Alternative to
support parsing. Use

  Left (e, s)   to indicate that input s was rejected with message e
  Right (x, s)  to indicate that x was successfully delivered, with
                  s the remainder of the input
----}

instance Applicative Parser where
  -- pure should succeed and consume no input
  -- <*> should parse the function, then the argument,
  --   and return the result of the application
	pure x = Parser (\ s -> Right (x, s))
	pf <*> pa = Parser (\ s -> case parse pf s of
															Right (f, s') -> case parse pa s' of
																									Right (a, s') -> Right (f a, s')
																									Left (msg, s') -> Left (msg, s')
															Left (msg, s) -> Left (msg, s)
											) 
--pf = parser function, f = function given by parser; pa = parser for argument, a = argument from pa applied

instance Alternative Parser where
  -- empty should reject the input with no message
  -- <|> should succeed if possible; if both arguments fail,
  --   some attempt should be made to choose the more reasonable
  --   explanation (how might the length of the rejected input
  --   play a part? how might the message?)
	--empty :: Parser x 
	empty = Parser (\ s -> Left ("", s)) 
	pf <|> pa = Parser(\ s -> case parse pf s of
														Right(f, s') -> Right (f, s')
														Left(msg, s') -> case parse pa s of -- (Couldn't match type `a' with `Parser a -> a' Right (f pa, s') --first succeeds so roll with it
																							Right (a, s'') -> Right (a, s'') --first succeeds so we win!
																							Left (e, s'') -> Left ("Tail end error:", comptails s' s'')													
										)
																							{-Left (msg, s') -> Left (msg, s')-}
comptails :: String -> String -> String
comptails s s' = if (length s) < (length s')
								 then s
								 else s'

{-
(<|>) :: Parser x ->  Parser x -> Parser x
Parser x  <|> _  = Parser x
Nothing <|> m  = m
-}


{- I've provided a Functor instance for you: -}

instance Functor Parser where
  fmap = (<*>) . pure

{- Define a parser for a single character which must satisfy a given test. -}

char :: (Char -> Bool) -> Parser Char
char f = Parser $  \s -> case s of
													[] -> Left ("Empty string: ", s)
													s -> case f (head s) of
															True -> Right (head s, tail s)
															False -> Left ("Char error:", s)

{----
What does

  traverse (char . (==))

do, e.g., if you apply it to "fred"?


Input: > parse (traverse (char . (==)) "fred") "fred"
Answer: It outputs 
					> Right ("fred", "")
----}

{----
Define a parser which replaces the empty error message with a better one.

message :: String -> Parser x -> Parser x
message = undefined

You should make sure that

  message "barf" empty <|> empty == empty <|> message "barf" empty

and that both deliver the message "barf" rather than no message at all.
----}

message :: String -> Parser x -> Parser x
--message e empty = Parser (\s -> Left (e, s))
message e p = Parser $ \s -> case parse p s of 
															Left(_, s) -> Left (e, s)
															Right r -> Right r 
--Got it working :)...finally!


{----
(2) Define parsers for tokens

Hint: you may find it useful to check out the operators

  many
  some

in the library documentation for Control.Applicative.
----}

space :: Parser ()
space = pure () <* many (char (isSpace)) 

{-Parser $ \s -> case isSpace s of
														True -> Right ((), s)-}														

variable :: Parser String
variable = pure (:) <*> char isLower <*> many (char isAlphaNum) 
  -- this should accept any sequence of alphanumeric characters
  -- beginning with a lower case letter

constructor :: Parser String
constructor = pure (:) <*> char isUpper <*> many (char isAlphaNum) 
  -- this should accept any sequence of alphanumeric characters
  -- beginning with an upper case letter

anyStartLetter :: Parser String
anyStartLetter = pure (:) <*> char isAlphaNum <*> (many (char isUpper) <|> many (char isLower))

{----
(3) Define a parser combinator for comma-separated lists in parentheses
----}

parenList :: Parser x -> Parser [x]
parenList p = parenThing (commaSep p)
  -- this should parse string (s1,s2,...,sn) as
  -- list [x1,x2,...,xn], if p parses each s as the corresponding x;
  -- be as liberal as you can about extra whitespace

--significant bug in code below...whitespace! so we must take this into consideration.
-- add <* space to the code :) 

parenThing :: Parser y -> Parser y
parenThing p = pure id	<* (punc '(') --char (== '(') -- :: Parser Char      -- noise								
												<*> p						 -- :: Parser y 			  -- signal												
												<* (punc ')') --char (== ')') -- :: Parser Char      -- noise

-- <$ is short hand for  (pure id <*)

commaSep :: Parser x -> Parser [x]
commaSep p =  pure (:) <*> p											
											 <*> many (pure id
																 <* (punc ',') --char (== ',')
																 <*> p
																 <* space
																)							
											 <|> pure [] <* space

punc :: Char -> Parser ()
punc c = message ("I want char: " ++ [c]) $ pure () 
					<* space 
					<* char (== c) 
					<* space
	
{----
(4) Define parsers for FOUL expressions and patterns
----}

{--- remember
data Expr
  = EC String [Expr]  -- should look like    Con(e1,...,en)
  | EV String        -- should look like    x
  | EA String [Expr]  -- should look like    f(e1,...en)
  deriving Show
----}

parseExpr :: Parser Expr
parseExpr = message "Expression expected"
						$ (EC <$> constructor <*> (parenList parseExpr <|> pure []))
						<|> (EA <$> variable <*> parenList parseExpr) 							
						<|> (EV <$> variable)				

{---- remember
data Pat
  = PC String [Pat]  -- should look like    Con(e1,...,en)
  | PV String        -- should look like    x
  deriving Show
----}

parsePat :: Parser Pat
parsePat = message "Pattern expected"
					 $ (PC <$> constructor <*> (parenList parsePat <|> pure [])
					 <|> PV <$> variable)

{----
(5) Define parsers for FOUL Line and whole FOUL programs
----}

{---- remember
type Line = ([Pat], Expr)
----}

parseLine :: Parser Line
parseLine = message "Line expected"
						$ ((,) <$> 
											parenList parsePat 
										<* punc '=' 
										<*> parseExpr)

{---- remember
type Prog = [(String, [Line])]
----}

parseProg :: Parser Prog
--parseProg = groupLines <$> (id <$ space <*> parseLine <* space)
parseProg = groupLines <$> (message "Program expected"
					  $ many ((,) <$ space <*> variable <*> parseLine))


groupLines :: [(FName, Line)] -> Prog
groupLines fls = [(f, [l | (g, l) <- fls, f==g])|f <- fnames] 
									where
									fnames = nub [f | (f, _) <- fls] --remove duplicate names here

--fnames removes duplicate function names
--fls = the of list of pairs of function name and line
-- next is a list comprehension which takes the function name, and another list which checks if f=g and this tells wether the function name is the same and displays the line, where f is function, l is line.


{----
(6) Wrap the whole thing in a function
----}

in2out :: String -> String
{-in2out s = case s of
						[] -> "PROGRAM FAILURE"
						s -> case parse parseProg s of
										Right r -> eval (EA "main" [])
										Left l -> "PROGRAM FAILURE"							 
-}

{-
in2out s = case ((parse parseProg) s) of
							Right (p, []) -> show (eval p [] (EA "main" []))
							Right (_, s) -> show ("Input expected: " ++ s)
							Left (e, s) ->	show ("error" ++ s)	 
-}


in2out s = case parse (message "Failure: Syntax!" parseProg) s of
						Left (e, s') -> show( e ++ " Left over: " ++ s')
						Right (c, s') -> case (myEval c [] (EA "main" [])) of
															Just x -> show x
															Nothing -> "Error!"

{----
which
  tries to parse its input as a FOUL program, and
    if all the input is successfully consumed,
      evaluates   EA "main" []
        and returns the output (you can use show for that)
    if the input does not parse or is not all consumed
      returns the error message

You'll need to paste in or otherwise link your FOUL interpreter.
----}

{----
(7) Try it!
----}

--main :: IO ()
--main = interact in2out

runFOUL :: IO ()
runFOUL = interact in2out

{----
If you compile this with ghc, you should get an executable
which takes FOUL programs from  stdin and sends the relevant response
to stdout!

  ghc --make -o foul Prac4.hs
  ./foul < mysort.foul
----}

--eval stuff ported from Prac3

myEval :: Prog -> Env -> Expr -> Maybe Val
myEval fs xvs (EC c es) = Just (\vs -> VC c vs) <*> myEvals fs xvs es
myEval fs xvs (EV x) = myFetch x xvs
myEval fs xvs (EA f es) = join (Just(\ls vs -> runfun ls vs ) <*> myFetch f fs <*> myEvals fs xvs es)
	where
    	runfun :: [Line] -> [Val] -> Maybe Val
    	runfun ((ps, e) : ls) vs = join (Just (\xvs -> myEval fs xvs e) <*> matches ps vs) <|> runfun ls vs

{- rip up and add it applicatively above. same for fetch -}

myEvals :: Prog -> Env -> [Expr] -> Maybe [Val]
myEvals fs xvs es = traverse (myEval fs xvs) es

{- We need that looker-upper function. -}

myFetch :: String -> [(String, x)] -> Maybe x
myFetch x ((y, v) : sxs)
	| x == y = Just v
	| otherwise = myFetch x sxs
myFetch _ _ = Nothing

join :: Maybe (Maybe x) -> Maybe x
join (Just (Just x))  = Just x
join _                = Nothing
