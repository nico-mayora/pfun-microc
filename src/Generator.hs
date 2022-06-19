-- LABORATORIO DE PROGRAMACION FUNCIONAL 2022
-- MODULO DE GENERACIÓN DE CÓDIGO DE MÁQUINA

module Generator where

import Data.Char (ord)
import Syntax
import MachineLang

-- Implementar
generate :: Program -> Code
generate (Program mb) = concatMap redir mb ++ [SKIP]
    where redir (Com stmt) = generateStmt stmt
          redir (Decl name) = []

generateStmt :: Stmt -> Code
generateStmt (StmtExpr expr) = generateExpr expr
generateStmt (If expr body1 body2) = codeExpr ++ [JMPZ jmp1] ++ codeBody1 ++ [JUMP jmp2] ++ codeBody2
  where codeExpr = generateExpr expr
        codeBody1 = concatMap generateStmt body1
        codeBody2 = concatMap generateStmt body2
        jmp1 = length codeBody1 + 2
        jmp2 = length codeBody2 + 1
generateStmt (While expr body) = codeExpr ++ [JMPZ jmp1] ++ codeBody ++ [JUMP jmp2]
  where codeExpr = generateExpr expr
        codeBody = concatMap generateStmt body
        jmp1 = length codeBody + 2
        jmp2 = - (length codeBody + 1 + length codeExpr)
generateStmt (PutChar expr) = generateExpr expr ++ [WRITE]

generateExpr :: Expr -> Code
generateExpr (Var name) = [LOAD name]

generateExpr (CharLit char) = [PUSH $ charToInt char]

generateExpr (NatLit numb) = [PUSH numb]

generateExpr GetChar = [READ]

generateExpr (Unary Not expr) = generateExpr expr ++ [PUSH 0, CMP, JMPZ 3, PUSH 0, JUMP 2, PUSH 1]
generateExpr (Unary Neg expr) = generateExpr expr ++ [NEG]

generateExpr (Binary Or expr1 expr2) = gExpr1 ++ [JMPZ jmp1] ++ gExpr1 ++ [JUMP jmp2] ++ gExpr2
  where gExpr1 = generateExpr expr1
        gExpr2 = generateExpr expr2
        jmp1 = length gExpr1 + 2
        jmp2 = length gExpr2 + 1
generateExpr (Binary And expr1 expr2) = gExpr1 ++ [JMPZ jmp1] ++ gExpr2 ++ [JUMP jmp2] ++ gExpr1
  where gExpr1 = generateExpr expr1
        gExpr2 = generateExpr expr2
        jmp1 = length gExpr2 + 2
        jmp2 = length gExpr1 + 1
generateExpr (Binary Equ expr1 expr2) = gExpr1 ++ gExpr2 ++ [CMP, JMPZ 3, PUSH 0, JUMP 2, PUSH 1]
  where gExpr1 = generateExpr expr1
        gExpr2 = generateExpr expr2
generateExpr (Binary Less expr1 expr2) = gExpr1 ++ gExpr2 ++ [CMP, PUSH 1, CMP, JMPZ 3, PUSH 0, JUMP 2, PUSH 1]
  where gExpr1 = generateExpr expr1
        gExpr2 = generateExpr expr2
generateExpr (Binary Plus expr1 expr2) = generateExpr expr2 ++ generateExpr expr1 ++ [ADD]
generateExpr (Binary Minus expr1 expr2) = generateExpr expr2 ++ generateExpr expr1 ++ [SUB]
generateExpr (Binary Mult expr1 expr2) = generateExpr expr2 ++ generateExpr expr1 ++ [MUL]
generateExpr (Binary Div expr1 expr2) = generateExpr expr2 ++ generateExpr expr1 ++ [DIV]
generateExpr (Binary Mod expr1 expr2) = generateExpr expr2 ++ generateExpr expr1 ++ [MOD]

generateExpr (Assign name expr) = generateExpr expr ++ [STORE name]

charToInt :: Char -> Integer
charToInt = toInteger . ord
