module CqlTypes where 
import CqlGrammar

--grammar:
--data Type = TyInt | TyBool | TyString

--data E = If E E E | For E E | Show E | ShowLn E | Concat E E | EqualTo E E 
--  | NotEqualTo E E | TrueTerm | FalseTerm | Seq E E | Var String | Index String Int 
--  | NullTerm | NumTerm Int | Value String | Sort deriving (Show,Eq)
--type Env = [(String,E)]

typeOf :: E -> Type

-- Primitives
typeOf ( NumTerm _ )  = TyInt

typeOf ( TrueTerm ) = TyBool

typeOf ( FalseTerm ) = TyBool

typeOf ( Value _ ) = TyString

typeOf ( Var _ ) = TyString

typeOf ( NullTerm ) = TyString

typeOf ( Sort ) = TyString

-- If Statement
typeOf ( If e1 e2 e3 )	| (typeOf e1) /= TyBool = error "If statement not type safe" 
						| t2 == t3 = t2
  where (TyBool,t2,t3) = (typeOf e1, typeOf e2, typeOf e3)
  
-- For Statement
typeOf ( For e1 e2 ) | (typeOf e1) /= TyString = error "For statement not type safe"
				   	 | otherwise = t2
  where (TyString,t2) = (typeOf e1, typeOf e2)

-- Show Statements
typeOf ( Show e ) | (typeOf e) /= TyString = error "Show statement not type safe"
				  | otherwise = TyString

typeOf ( ShowLn e ) | (typeOf e) /= TyString = error "ShowLn statement not type safe"
				    | otherwise = TyString
  
-- Concatenate
typeOf ( Concat e1 e2 ) | ((typeOf e1) == TyString) && ((typeOf e2) == TyString) = TyString
						| otherwise = error "Concatenate statement not type safe"
  
-- Comparison
typeOf ( EqualTo e1 e2 ) | t1 == t2 = TyBool
						 | otherwise = error "Equal to statement not type safe"
  where (t1,t2) = (typeOf e1, typeOf e2)
  
typeOf ( NotEqualTo e1 e2 ) | t1 == t2 = TyBool
							| otherwise = error "Not equal to statement not type safe"
  where (t1,t2) = (typeOf e1, typeOf e2)
  
-- Sequence Statements
typeOf ( Seq e1 e2 ) | t1 == t2 = t1
					 | otherwise = error "Sequence statement not type safe"
  where (t1,t2) = (typeOf e1, typeOf e2)

-- Index Statements (already type checked within grammar rules)
typeOf ( Index s i ) = TyString

typeOf _ = error "Type Error"

-- Function for printing the results of the TypeCheck
unparseType :: Type -> String
unparseType TyBool = "Bool"
unparseType TyInt = "Int"
unparseType TyString = "String"
