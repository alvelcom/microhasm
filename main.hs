module Main (main) where

import Assembler



rF :: MAsm ()
rF = 
  do
-----------------------  
-- Main function
-----------------------
     label "main"
     
     inI Ax
     push Ax
     call "factorial" -- 3
     add Sp $ Num 1
 
     outI Ax

     call "end"
     
-----------------------
-- FACTORIAL
-- 1st arg is number position in sequence 
----------------------
{-
   if arg > 2 goto _rec
   ax = 1
   return

   _rec:
   bx = arg
   bx = bx - 1
   ax = factorial(bx)
   push ax
   bx = bx - 1
   ax = factorial(bx)
   pop bx

   return ax + bx
-}
     label "factorial"
     jg (Ptr Sp 2) (Num 2) "factorial_rec" -- 7
     mov Ax $ Num 1
     ret
     label "factorial_rec"

     mov Bx $ (Ptr Sp 2)
     dec Bx $ Num 1
     push Bx
     call "factorial"
     pop Bx
     push Ax
     dec Bx $ Num 1
     push Bx
     call "factorial"
     pop Bx
     pop Bx
     add Ax Bx
     ret
     

-----------------------
     label "end"
     mov Ax Ax


fF :: MAsm ()
fF = 
  do 
     mov Ax $ Num 1
     mov Bx $ Num 1
     inI Cx

     label "lbl"
     mov Dx Bx
     add Dx Ax
     mov Ax Bx
     mov Bx Dx
     dec Cx $ Num 1

     jne Cx Nil "lbl"

     outI Dx
     

main :: IO ()     
main = 
  do 
     execute 16000 rF
     return ()
