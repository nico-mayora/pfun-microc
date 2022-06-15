-- LABORATORIO DE PROGRAMACION FUNCIONAL 2022
-- MODULO DE OPTIMIZACIÓN

module Optimizer where

import Syntax


-- Implementar
optimize :: Program -> Program
optimize (Program mb) =  Program [optim stmt | stmt <- mb]
  where optim (Com stmt) = Com $ folding stmt
        -- optim (Com stmt) = Com $ eliminateDeadCode . folding stmt ORIGINAL,.changed for testing
        optim (Decl varDecl) = Decl varDecl

folding :: Stmt -> Stmt
folding (StmtExpr expr) = StmtExpr $ foldExpr expr
folding (If expr body1 body2) = If (foldExpr expr) body1 body2
folding (While expr body) = While (foldExpr expr) body
folding (PutChar expr) = PutChar $ foldExpr expr

foldExpr :: Expr -> Expr
foldExpr (Unary uOp expr) = Unary uOp expr -- Optimizar?? no esta en la letra
foldExpr (Binary bOp expr1 expr2) =
  case (foldExpr1, foldExpr2, bOpNeutro) of 
       (neutroBOp, _, True) -> foldExpr2
       (_, neutroBOp, True) -> foldExpr1 -- redundant???? kek idk why the error
       _ -> if esNulo foldExpr1 bOp then
              foldExpr1
            else if esNulo foldExpr2 bOp then
                foldExpr2
              else
                reduce bOp foldExpr1 foldExpr2
  where foldExpr1 = foldExpr expr1
        foldExpr2 = foldExpr expr2
        neutroBOp = neutro bOp
        bOpNeutro = bOp == Plus || bOp == Mult || bOp == And || bOp == Or
        
foldExpr (Assign name expr) = Assign name (foldExpr expr)
--foldExpr (Var name) = Var name
--foldExpr (CharLit char) = CharLit char
--foldExpr (NatLit integer) = NatLit integer
--foldExpr GetChar = undefined
foldExpr expr = expr

neutro :: BOp -> Expr
neutro Plus = NatLit 0
neutro Mult = NatLit 1
neutro And = NatLit 1
neutro Or = NatLit 0
neutro _ = NatLit 0 -- da igual ya que nunca va a matchear

esNulo :: Expr -> BOp -> Bool
esNulo (NatLit num) Mult = num == 0
esNulo (NatLit num) And = num == 0
esNulo (NatLit num) Or = num /= 0
esNulo _ _ = False

reduce :: BOp -> Expr -> Expr -> Expr
reduce Plus (NatLit num1) (NatLit num2) = NatLit (num1 + num2)
reduce Minus (NatLit num1) (NatLit num2) = NatLit (num1 - num2) -- agregar unari Op - si es negativo?
reduce Mult (NatLit num1) (NatLit num2) = NatLit (num1 * num2)
reduce Div (NatLit num1) (NatLit num2) = NatLit (num1 `div` num2)
reduce Mod (NatLit num1) (NatLit num2) = NatLit (num1 `mod` num2)
reduce bOp expr1 expr2 = Binary bOp expr1 expr2

-------------------------------------------------------------------

eliminateDeadCode :: Stmt -> Stmt
eliminateDeadCode = undefined


