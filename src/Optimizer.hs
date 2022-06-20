-- LABORATORIO DE PROGRAMACION FUNCIONAL 2022
-- MODULO DE OPTIMIZACIÓN

module Optimizer where

import Syntax


-- Implementar
optimize :: Program -> Program
optimize (Program mb) =  Program $ concatMap (optim2 . optim1) mb
  where optim1 (Com stmt) = Com $ folding stmt
        optim1 compStat = compStat
        optim2 (Com stmt) =
          let elim = eliminateDeadCode stmt
           in if null elim then
                []
              else
                map Com elim
        optim2 compStat = [compStat]

folding :: Stmt -> Stmt
folding (StmtExpr expr) = StmtExpr $ foldExpr expr
folding (If expr body1 body2) = If (foldExpr expr) (map folding body1) (map folding body2)
folding (While expr body) = While (foldExpr expr) (map folding body)
folding (PutChar expr) = PutChar $ foldExpr expr

foldExpr :: Expr -> Expr
foldExpr (Binary bOp expr1 expr2)
  | esNeutro foldExpr1 bOp = foldExpr2
  | esNeutro foldExpr2 bOp = foldExpr1
  | tieneAsig expr1 || tieneAsig expr2 = Binary bOp foldExpr1 foldExpr2
  | esNulo foldExpr1 bOp = foldExpr1
  | esNulo foldExpr2 bOp = foldExpr2
  | otherwise = reduce bOp foldExpr1 foldExpr2
  where foldExpr1 = foldExpr expr1
        foldExpr2 = foldExpr expr2        
foldExpr (Assign name expr) = Assign name (foldExpr expr)
foldExpr (Unary Neg expr) = 
  case exprFolded of
       (Unary Neg num) -> num
       _ -> exprFolded
  where exprFolded = foldExpr expr
foldExpr expr = expr

esNeutro :: Expr -> BOp -> Bool
esNeutro (NatLit num) Plus = num == 0
esNeutro (NatLit num) Mult = num == 1
esNeutro (NatLit num) And = num /= 0
esNeutro (NatLit num) Or = num == 0
esNeutro _ _ = False

esNulo :: Expr -> BOp -> Bool
esNulo (NatLit num) Mult = num == 0
esNulo (NatLit num) And = num == 0
esNulo (NatLit num) Or = num /= 0
esNulo _ _ = False

reduce :: BOp -> Expr -> Expr -> Expr
reduce Plus (NatLit num1) (NatLit num2) = NatLit (num1 + num2)
reduce Minus (NatLit num1) (NatLit num2) =
  let res = num1 - num2
   in if res < 0 then
        Unary Neg (NatLit $ abs res)
      else
        NatLit res
reduce Mult (NatLit num1) (NatLit num2) = NatLit (num1 * num2)
reduce Div (NatLit num1) (NatLit num2) = NatLit (num1 `div` num2)
reduce Mod (NatLit num1) (NatLit num2) = NatLit (num1 `mod` num2)
reduce bOp expr1 expr2 = Binary bOp expr1 expr2


tieneAsig :: Expr -> Bool
tieneAsig (Assign _ _) = True
tieneAsig (Unary    _ expr) = tieneAsig expr
tieneAsig (Binary _ expr1 expr2) = tieneAsig expr1 || tieneAsig expr2
tieneAsig _ = False

------------------------------------------------------------------- 

eliminateDeadCode :: Stmt -> [Stmt]
eliminateDeadCode (While (NatLit num) body) =  [While (NatLit num) (concatMap eliminateDeadCode body) | num /= 0]
eliminateDeadCode (While expr body) =  [While expr (concatMap eliminateDeadCode body)]
eliminateDeadCode (If (NatLit num) body1 body2) = 
  if num /= 0 then
    concatMap eliminateDeadCode body1 
  else 
    concatMap eliminateDeadCode body2
eliminateDeadCode (If expr body1 body2) = [If expr (concatMap eliminateDeadCode body1) (concatMap eliminateDeadCode body2)]
eliminateDeadCode stmt = [stmt]
