-- LABORATORIO DE PROGRAMACION FUNCIONAL 2022
-- MODULO DE CHEQUEO DE NOMBRES Y TIPOS

module TypeChecker where

import Distribution.TestSuite (Result (Error))
import Syntax
import System.Posix.Internals (newFilePath)

data Error
  = Duplicated Name
  | Undefined Name
  | Expected Type Type

instance Show Error where
  show (Duplicated n) = "Duplicated definition: " ++ n
  show (Undefined n) = "Undefined: " ++ n
  show (Expected ty ty') =
    "Expected: " ++ show ty
      ++ " Actual: "
      ++ show ty'

type Env = [(Name, Type)]

-- Implementar
checkProgram :: Program -> [Error]
checkProgram prg =
  if null nameErrors
    then typeErrors
    else nameErrors
  where
    (env, nameErrors) = checkNames prg []
    typeErrors = checkTypes env prg

type EnvError = (Env, [Error])

checkNames :: Program -> Env -> EnvError
checkNames (Program mb) env = foldl processLine (env, []) mb
  where
    processLine envErrors (Com stmt) = (fst envErrors, checkNamesStmt envErrors stmt)
    processLine envErrors (Decl varDecl) = checkNamesDecl envErrors varDecl

checkNamesStmt :: EnvError -> Stmt -> [Error]
checkNamesStmt (env, errors) (StmtExpr expr) = errors ++ checkExpr env expr
checkNamesStmt (env, errors) (If expr body1 body2) = checkExpr env expr ++ checkNamesBody1 ++ checkNamesBody2
  where
    checkNamesBody1 = foldl (\x -> checkNamesStmt (env, x)) [] body1
    checkNamesBody2 = foldl (\x -> checkNamesStmt (env, x)) [] body2
checkNamesStmt (env, errors) (While expr body) = checkExpr env expr ++ checkNamesBody
  where
    checkNamesBody = foldl (\x -> checkNamesStmt (env, x)) [] body
checkNamesStmt (env, errors) (PutChar expr) = errors ++ checkExpr env expr

checkExpr :: Env -> Expr -> [Error]
checkExpr env (Var name) = [Undefined name | null $ usedInEnv env name]
checkExpr env (Unary _ expr) = checkExpr env expr
checkExpr env (Binary _ expr1 expr2) = checkExpr env expr1 ++ checkExpr env expr2
checkExpr env (Assign name expr) = if null $ usedInEnv env name then Undefined name : checkExpr env expr else checkExpr env expr
checkExpr _ _ = []

checkNamesDecl :: EnvError -> VarDef -> EnvError
checkNamesDecl (env, errors) (VarDef varType name) =
  if null $ usedInEnv env name
    then ((name, varType) : env, errors)
    else (env, errors ++ [Duplicated name])

usedInEnv :: Env -> String -> Env
usedInEnv env name = filter (\x -> fst x == name) env

type TypeError = (Type, [Error])

checkTypes :: Env -> Program -> [Error]
checkTypes env (Program mb) = snd $ foldl processLine (env, []) mb
  where
    processLine envErrors (Com stmt) = processStmt envErrors stmt
    processLine envErrors _ = envErrors

errBody :: Env -> Body -> [Error]
errBody env stmts = snd $ foldl processStmt (env, []) stmts

processStmt :: EnvError -> Stmt -> EnvError
processStmt (env, errs) (StmtExpr expr) = (env, errs ++ snd (expType env expr))
processStmt (env, errs) (If expr body1 body2) =
  let expTup = expType env expr
   in (env, errs ++ snd expTup ++ [Expected TyInt TyChar | fst expTup /= TyInt] ++ errBody env body1 ++ errBody env body2)
processStmt (env, errs) (While expr body) =
  let expTup = expType env expr
   in (env, errs ++ snd expTup ++ [Expected TyInt TyChar | fst expTup /= TyInt] ++ errBody env body)
processStmt (env, errs) (PutChar expr) =
  let expTup = expType env expr
   in (env, errs ++ snd expTup ++ [Expected TyChar TyInt | fst expTup == TyInt])

expType :: Env -> Expr -> TypeError
expType env (Var name) = (snd . head $ filter (\x -> fst x == name) env, [])
expType env (CharLit _) = (TyChar, [])
expType env (NatLit _) = (TyInt, [])
expType env GetChar = (TyChar, [])
expType env (Unary _ exp) =
  let expTup = expType env exp
   in (TyInt, snd expTup ++ [Expected TyInt TyChar | fst expTup /= TyInt])
expType env (Binary bop exp1 exp2) -- TODO: revisar cantidad & orden de errores
  | bop == Less || bop == Equ =
    let err = case (fst expTup1, fst expTup2) of
          (TyInt, TyChar) -> [Expected TyInt TyChar]
          (TyChar, TyInt) -> [Expected TyChar TyInt]
          _ -> []
     in (TyInt, snd expTup1 ++ snd expTup2 ++ err)
  | otherwise =
    let err = case (fst expTup1, fst expTup2) of
          (TyInt, TyInt) -> []
          (TyChar, TyChar) -> [Expected TyInt TyChar, Expected TyInt TyChar]
          _ -> [Expected TyInt TyChar]
     in (TyInt, snd expTup1 ++ snd expTup2 ++ err)
  where
    expTup1 = expType env exp1
    expTup2 = expType env exp2
expType env (Assign name exp) =
  let nameType = snd . head $ filter (\x -> fst x == name) env
      expTup = expType env exp
   in (nameType, snd expTup ++ [Expected nameType $ fst expTup | nameType /= fst expTup])
