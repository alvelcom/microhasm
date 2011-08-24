module Main (main) where

import Assembler



asm :: MAsm ()
asm = 
  do
     mov Sp $ Num 16000  
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
     jg (Aptr Sp 1) (Num 2) "factorial_rec" -- 7
     mov Ax $ Num 1
     ret
     label "factorial_rec"

     mov Bx $ (Aptr Sp 1)
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

     

main :: IO ()     
main = 
  do 
     putStrLn $ show $ getAsm asm
     execute 16000 asm
     return ()
