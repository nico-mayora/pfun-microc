-- LABORATORIO DE PROGRAMACION FUNCIONAL 2022
-- MODULO DEL INTÉRPRETE DEL LENGUAJE DE MÁQUINA

module Interpreter where

import Data.Char (chr, ord)
import GHC.IO.Exception (stackOverflow)
import MachineLang

type Conf = (Stack, Env)

type Env = [(Var, Integer)]

type Stack = [Integer]

-- Implementar
interp :: Code -> Code -> Conf -> IO Conf
interp _ [] final_state = return final_state
interp before after@(JUMP shft : rest) state = performJump before after shft state
interp before after@(JMPZ shft : rest) (stack, env) = do
  let conditional = head stack
  if conditional == 0
    then performJump before after shft (tail stack, env)
    else interp (head after : before) (tail after) (tail stack, env)
interp before after state = do
  new_state <- interpJustOne (head after) state
  interp (head after : before) (tail after) new_state

performJump :: Code -> Code -> Int -> Conf -> IO Conf
performJump before after shft state =
  if shft > 0
    then do
      -- jump forwards
      let (skipped, interpNext) = splitAt shft after
      let reversed = reverse skipped
      interp (reversed ++ before) interpNext state
    else do
      -- jump backwards
      let (skipped, new_before) = splitAt (abs shft) before
      let reversed = reverse skipped
      interp new_before (reversed ++ after) state

interpJustOne :: Instr -> Conf -> IO Conf
interpJustOne NEG (stack, env) = do
  let popped = head stack
  return ((- popped) : tail stack, env)
interpJustOne ADD (stack, env) = return (applyBinOper (+) stack, env)
interpJustOne SUB (stack, env) = return (applyBinOper (-) stack, env)
interpJustOne MUL (stack, env) = return (applyBinOper (*) stack, env)
interpJustOne DIV (stack, env) = return (applyBinOper div stack, env)
interpJustOne MOD (stack, env) = return (applyBinOper mod stack, env)
interpJustOne CMP (stack, env) = do
  let (op1, op2, newStack) = popTwoFromStack stack
  let ret = case compare op1 op2 of
        LT -> ((-1) : newStack, env) 
        EQ -> (0 : newStack, env) 
        GT -> (1 : newStack, env)
  return ret
interpJustOne (PUSH int) (stack, env) = return (int : stack, env)
interpJustOne (LOAD var) (stack, env) = return (snd (head $ filter (\x -> fst x == var) env) : stack, env)
interpJustOne (STORE var) (stack, env) = return (tail stack, (var, head stack) : filter (\x -> fst x /= var) env)
interpJustOne READ (stack, env) = do
  c <- getChar
  let asciiCode = toInteger . ord $ c
  return (asciiCode : stack, env)
interpJustOne WRITE (stack, env) = do
  let charToPrint = chr . fromIntegral . head $ stack
  putChar charToPrint
  return (tail stack, env)
interpJustOne (JUMP sft) cnf = undefined
interpJustOne (JMPZ sft) cnf = undefined
interpJustOne SKIP cnf = return cnf

popTwoFromStack :: Stack -> (Integer, Integer, Stack)
popTwoFromStack sck = (head sck, sck !! 1, drop 2 sck)

applyBinOper :: (Integer -> Integer -> Integer) -> Stack -> Stack
applyBinOper fn stack = fn op1 op2 : drop 2 stack
  where
    op1 = head stack
    op2 = stack !! 1

-- getIntCmp :: 