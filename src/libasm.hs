module LibAsm where

import Assembler
import Prelude (($))
import qualified Prelude as P



libAsm :: MAsm ()
libAsm
 = do push Sp
      call "initHeap"
      pop Nil
      call "main"

----------------------
      label "initHeap"
     
      mov Ax       $ Ptr Sp 1
      mov (Offset 1) Ax
      div Ax       $ Num 32

      push $ Num 0
      push Ax
      push $ Num 1
      call "memfill"
      add Sp       $ Num 3

      ret 

--------------------
      label "memfill"

      mov Ax       $ Ptr Sp 2 -- Ptr to #0
      mov Bx       $ Ptr Sp 3 -- Size of zone
      add Bx         Ax
      mov Dx       $ Ptr Sp 4 -- Fill element

      label "memfill_cycle"
      mov (Ref Ax)  Dx
      inc Ax
      jl  Ax        Bx "memfill_cycle"

      ret
      
-------------------      
      label "memcpy"

      mov Ax       $ Ptr Sp 2
      mov Bx       $ Ptr Sp 3
      mov Cx       $ Ptr Sp 4

      label "memcpy_cycle"
      decr Cx
      mov (Ref Bx) $ Ref Ax
      inc Ax
      inc Bx
      jg Cx (Num 0) "memcpy_cycle"

      ret

-------------------
      label "memcpy'"

      mov Ax       $ Ptr Sp 2
      mov Bx       $ Ptr Sp 3
      mov Cx       $ Ptr Sp 4

      add Ax         Cx
      add Bx         Cx
      
      label "memcpy'_cycle"
      decr Cx
      decr Ax
      decr Bx
      mov (Ref Bx) $ Ref Ax
      jg Cx (Num 0) "memcpy'_cycle"

      ret
      
------------------
      label "memmove"

      mov Ax       $ Ptr Sp 2 -- from 
      mov Bx       $ Ptr Sp 3 -- to 
      mov Cx       $ Ptr Sp 4 -- size

      je Ax          Bx "memmove_end"
      je (Num 0)     Cx "memmove_end"

      mov Dx         Ax
      add Dx         Cx
      jl  Bx         Dx "memmove_intersect"

      call "memcpy"  -- call memcpy with same arguments as 
                     -- far as we didn't pop anything!

      label "memmove_end"
      ret

      label "memmove_intersect"
      call "memcpy'"
      ret

------------------      
      label "getbit"

      mov Ax       $ Ptr Sp 2 -- bitmask
      mov Bx       $ Ptr Sp 3 -- # bit

      jg  Bx       (Num 31) "getbit_fail"
      jl  Bx         Nil    "getbit_fail"
      mov Cx       $ Num 31
      dec Cx         Bx
      shl Ax         Cx
      shr Ax       $ Num 31
      ret                     -- see Ax for result

      label "getbit_fail"
      mov Ax         Nil
      ret

-------------------
      label "setbit"

      mov Ax       $ Ptr Sp 2 -- bitmask
      mov Bx       $ Ptr Sp 3 -- # bit
      mov Cx       $ Ptr Sp 4 -- value

      jg  Bx       (Num 31) "getbit_fail"
      jl  Bx         Nil    "getbit_fail"      
      je  Cx         Nil    "setbit_neg"

      mov Cx       $ Num 1      
      shl Cx         Bx
      or  Ax         Cx 
      ret

      label "setbit_neg"
      mov Dx         Ax
      mov Cx       $ Num 32
      dec Cx         Bx
      shl Dx         Cx
      shr Dx         Cx

      inc Bx
      shr Ax         Bx
      shl Ax         Bx

      or Ax          Dx
      ret

      






















      
