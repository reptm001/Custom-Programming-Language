import CqlTokens
import CqlGrammar
import CqlEvaluator
import CqlTypes
import System.Environment
import Control.Exception
import System.IO


main :: IO()
main = catch parsing exception


exception :: ErrorCall -> IO()
exception e = do
	let err = show e
	hPutStrLn stderr ("Problem occured " ++ err) 
	return ()


parsing :: IO()
parsing = do
	(filename:_) <- getArgs
	input <- readFile filename
	
	let fs = (fst (break ('\n'==) input))
	let temp = tail (snd (break ('\n'==) input))
	let vs = fst(break ('\n'==) temp)
	let code = tail (snd (break ('\n'==) temp))
	
	s <- split fs
	f <- mapM parseCSV s
	v <- split vs
	let ff = map (removeFWhitespace) f
	
	--putStrLn ("Parsing : " ++ code)
	let lexResult = alexScanTokens code
	--putStrLn ("Lexed result : " ++ (show lexResult))
	let parseResult = parseFunc lexResult
	--putStrLn ("Parsed result : " ++ (show parseResult))
	let typeResult = typeOf parseResult
	--putStrLn (unparseType typeResult)
	let result = repeatedEval parseResult (createEnv (v) ff)
	putStr (unparse result)


removeFWhitespace f
	| (elem ',' f) =  (removeTrailingWhitespace(removeLeadingWhitespace((fst (break (','==) f))))) ++ "," ++  (removeFWhitespace (tail(snd (break (','==) f))))
	| otherwise = (removeTrailingWhitespace(removeLeadingWhitespace(f)))

removeLeadingWhitespace x
	| length x == 0 = ""
	| (head x) == ' ' = removeLeadingWhitespace (tail x)
	| (head x) == '\t' = removeLeadingWhitespace (tail x)
	| otherwise = x

removeTrailingWhitespace x
	| length x == 0 = ""
	| (last x) == ' ' = (removeTrailingWhitespace (init x))
	| (last x) == '\t' = (removeTrailingWhitespace (init x))
	| otherwise = x

createEnv :: [String] -> [String] -> Env
createEnv [] [] = []
createEnv (v:vs) (f:fs) = (v, (Value f)) : (createEnv vs fs)


split xs
	| (elem ',' xs) = do return( (fst (break (','==) xs)) : (splitIntoList'(tail (snd (break (','==) xs)))))
	| otherwise = do return ([xs])


csvToString xs
	| length xs == 0 = ""
	| length xs == 1 = (head xs)
	| otherwise = (head xs) ++ "\n" ++ (csvToString (tail xs))


splitIntoList x = map (splitIntoList') (lines x)
splitIntoList' xs
	| (elem ',' xs) = [(fst (break (','==) xs))] ++ (splitIntoList'(tail (snd (break (','==) xs))))
	| otherwise = [xs]


parseCSV fileName = do
	file <- readFile fileName
	return (csvToString (lines file))