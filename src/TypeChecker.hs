-- LABORATORIO DE PROGRAMACION FUNCIONAL 2022
-- MODULO DE CHEQUEO DE NOMBRES Y TIPOS

module TypeChecker where

import Syntax
import Distribution.TestSuite (Result(Error))
import System.Posix.Internals (newFilePath)


data Error = Duplicated      Name
           | Undefined       Name
           | Expected        Type Type

instance Show Error where
   show (Duplicated      n)  = "Duplicated definition: " ++ n
   show (Undefined       n)  = "Undefined: " ++ n
   show (Expected    ty ty') = "Expected: " ++ show ty
                             ++ " Actual: " ++ show ty'

type Env = [(Name, Type)]

getProg :: Either String Program -> Program 
getProg (Right p) = p
getProg (Left _) = Program []

-- Implementar
checkProgram :: Program -> [Error]
checkProgram prg =   if null nameErrors
                        then typeErrors
                        else nameErrors
                     where nameErrors = checkNames prg []
                           typeErrors = checkTypes prg

type EnvError = (Env, [Error])

checkNames :: Program -> Env -> [Error]
checkNames (Program mb) env = snd $ foldl processLine (env, []) mb
   where processLine envErrors (Com stmt) = (fst envErrors, checkNamesStmt envErrors stmt)
         processLine envErrors (Decl varDecl) = checkNamesDecl envErrors varDecl

checkNamesStmt :: EnvError -> Stmt -> [Error]
checkNamesStmt (env, errors) (StmtExpr expr) = errors ++ checkExpr env expr
checkNamesStmt (env, errors) (If expr body1 body2) = checkExpr env expr ++ checkNamesBody1 ++ checkNamesBody2
   where checkNamesBody1 = foldl (\x -> checkNamesStmt (env, x)) [] body1
         checkNamesBody2 = foldl (\x -> checkNamesStmt (env, x)) [] body2
checkNamesStmt (env, errors) (While expr body) = checkExpr env expr ++ checkNamesBody
   where checkNamesBody = foldl (\x -> checkNamesStmt (env, x)) [] body
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
      then ((name, varType):env, errors)
      else (env, errors++[Duplicated name])

usedInEnv :: Env -> String -> Env
usedInEnv env name = filter (\x -> fst x == name) env



checkTypes :: Program -> [Error]
checkTypes _ = []