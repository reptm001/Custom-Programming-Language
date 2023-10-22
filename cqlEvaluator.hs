module CqlEvaluator where
import CqlGrammar
import Data.List.Split
import Data.List


data Frame = HFor E Int | HIf E E | HEquals E | EqualsH E | HConcat E | ConcatH E | HSeq E | SeqH E | HEqualTo E | EqualToH E | HNotEqualTo E | NotEqualToH E | ShowLnH | ShowH
type K = [Frame]

type State = (E,Env,K)


unparse :: E -> String
unparse (Var s) = s
unparse (Value s) = s
unparse (NumTerm n) = show n
unparse TrueTerm = "true"
unparse FalseTerm = "false"
unparse NullTerm = "null"
unparse _ = "unknown"


getBinding :: String -> Env -> E
getBinding x [] = error (x ++ " is not bound in environment")
getBinding x ((s,e):env) | s == x = e
						 | otherwise = getBinding x env


isValue :: E -> Bool
isValue (NumTerm _) = True
isValue (Var _) = True
isValue (Value _) = True
isValue FalseTerm = True
isValue TrueTerm = True
isValue NullTerm = True
isValue _ = False


repeatedEval e env = loop (e, ("output",(Value "")):env, []) --first expression
	
loop (e, env, k) = do
	let (e',env',k') = eval (e,env,k)
	if (e' == e) && (isValue e') && (null k) then e' else loop (e',env',k')


isEmpty :: String -> Env -> Bool
isEmpty s env | result == "" = True
              | otherwise = False
    where (Value result) = (getBinding s env)

eval :: State -> State
--variable rule (v, Env, K)
eval ((Var x), env, k) = (getBinding x env, env, k)
	--get the value that x is bound to in the environment


--termination rule (v, Env, []), no more continuations
eval c@(e, env, []) | isValue e = c


--for rules   
eval ((For (Var s) e), env, k) | (isEmpty s env) = (NullTerm, env, k) 
                               | otherwise = (e, [(s++"ForE",e)] ++ [(s++"Row",Value "1")] ++ (getRow s 1 env) ++ env, (HFor (Var s) 1):k)
eval (e, env, (HFor (Var s) n):k) | isValue e && n < (getRows s env) = (rowE, (getRow s (n+1) env) ++ [(s++"Row",(Value (show (n+1))))] ++ env, (HFor (Var s) (n+1)): k)
                                  | isValue e && n == (getRows s env) = (e, env, k)
    where rowE = getBinding (s++"ForE") env


--index rules
eval ((Index s i), env, k) = (getColumn result i, env, k)
    where (Value result) = getBinding (s ++ rowNum) env
          (Value rowNum) = getBinding (s++"Row") env

--if rules
eval ((If e1 e2 e3), env, k) = (e1, env, (HIf e2 e3):k)
eval (TrueTerm, env, (HIf e1 e2):ks) = (e1, env, ks)
eval (FalseTerm, env, (HIf e1 e2):ks) = (e2, env, ks)


--concatenation rules
eval ((Concat e1 e2), env, k) = (e1, env, (HConcat e2):k)
eval (x@(Value s), env, (HConcat e):ks) = (e, env, (ConcatH x):ks)
eval ((Value s), env, (ConcatH (Value s1):ks)) = (Value (s1 ++ "," ++ s), env, ks)


--comparison rules (equal/not equal to)
eval ((EqualTo e1 e2), env, k) = (e1, env, (HEqualTo e2):k)
eval (x@(NumTerm n), env, (HEqualTo e):ks) = (e, env, (EqualToH x):ks)
eval (x@(Value s), env, (HEqualTo e):ks) = (e, env, (EqualToH x):ks)
eval (x@(TrueTerm), env, (HEqualTo e):ks) = (e, env, (EqualToH x):ks)
eval (x@(FalseTerm), env, (HEqualTo e):ks) = (e, env, (EqualToH x):ks)
eval (x1@(NumTerm n), env, (EqualToH x2):ks) 
    | x1 == x2 = (TrueTerm, env, ks)
    | otherwise = (FalseTerm, env, ks)
eval (x1@(Value s), env, (EqualToH x2):ks) 
    | x1 == x2 = (TrueTerm, env, ks)
    | otherwise = (FalseTerm, env, ks)
eval (x1@(TrueTerm), env, (EqualToH x2):ks) 
    | x1 == x2 = (TrueTerm, env, ks)
    | otherwise = (FalseTerm, env, ks)
eval (x1@(FalseTerm), env, (EqualToH x2):ks) 
    | x1 == x2 = (TrueTerm, env, ks)
    | otherwise = (FalseTerm, env, ks)


eval ((NotEqualTo e1 e2), env, k) = (e1, env, (HNotEqualTo e2):k)
eval (x@(NumTerm n), env, (HNotEqualTo e):ks) = (e, env, (NotEqualToH x):ks)
eval (x@(Value s), env, (HNotEqualTo e):ks) = (e, env, (NotEqualToH x):ks)
eval (x@(TrueTerm), env, (HNotEqualTo e):ks) = (e, env, (NotEqualToH x):ks)
eval (x@(FalseTerm), env, (HNotEqualTo e):ks) = (e, env, (NotEqualToH x):ks)
eval (x1@(NumTerm n), env, (NotEqualToH x2):ks) 
    | x1 /= x2 = (TrueTerm, env, ks)
    | otherwise = (FalseTerm, env, ks)
eval (x1@(Value s), env, (NotEqualToH x2):ks) 
    | x1 /= x2 = (TrueTerm, env, ks)
    | otherwise = (FalseTerm, env, ks)
eval (x1@(TrueTerm), env, (NotEqualToH x2):ks) 
    | x1 /= x2 = (TrueTerm, env, ks)
    | otherwise = (FalseTerm, env, ks)
eval (x1@(FalseTerm), env, (NotEqualToH x2):ks) 
    | x1 /= x2 = (TrueTerm, env, ks)
    | otherwise = (FalseTerm, env, ks)


--showln rules
eval ((ShowLn e), env, k) = (e, env, (ShowLnH:k))
eval (e, env, (ShowLnH):ks) | isString e = ((Value (result ++ (getString e))), ("output", (Value (result ++ (getString e) ++ "\n"))):env, ks)
	where (Value result) = getBinding "output" env


--show rules
eval ((Show e), env, k) = (e, env, (ShowH:k))
eval (NullTerm, env, (ShowH):ks) = (NullTerm, env, ks)
eval (e, env, (ShowH):ks) | isString e = ((Value (result ++ (getString e))), ("output", (Value (result ++ (getString e) ++ ","))):env, ks)
	where (Value result) = getBinding "output" env


--sort rules
eval (Sort, env, k) = (Value (sortOutput result), ("output", Value (sortOutput result)):env, k)
	where (Value result) = getBinding "output" env

--sequence rules THIS NEEDS TO BE THE LAST RULE TO EVALUATE
eval ((Seq e1 e2), env, k) = (e1, env, (HSeq e2):k)
eval (e1, env, (HSeq e2):ks) = (e2, env, (SeqH e1):ks)
eval (e1, env, ((SeqH e2):ks)) = (e1, env, ks)


--error rule
eval (e,env,k) = error "Error with evaluating"


getRows :: String -> Env -> Int
getRows var env = ((length . filter (=='\n')) result) + 1
    where (Value result) = getBinding var env

--("a",(Value "3,4\n3,5")):env -> ("a1",(Value "3,4")):("a2",(Value "3,5")):env 
getRow :: String -> Int -> Env -> Env
getRow var row env = [((var ++ (show row)),(Value ((splitOn "\n" result)!!(row-1))))]
    where (Value result) = getBinding var env
 
--("a1",(Value "3,4")):env -> "3,4" -> i = 2, => (Value "4")
getColumn :: String -> Int -> E
getColumn s i = (Value ((splitOn "," s)!!(i-1)))


getString :: E -> String
getString (Value s) = s

isString :: E -> Bool
isString (Value _) = True
isString (NullTerm) = False
isString _ = False


sortOutput :: String -> String
sortOutput s = intercalate "\n" (sort splitted)
	where splitted | s == "" = splitOn "\n" s
                   | otherwise = splitOn "\n" (init s)



