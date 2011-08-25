module Assembler where


import qualified Data.Map as M
import Data.Maybe
import Data.Array as A
import Data.Array.IArray as I
import Data.Array.IO
import Control.Monad


-- Base types
data Assembler = Assembler { 
                            cmds  :: [Cmd],
                            dicts :: (M.Map String Int)
                           }

type MT = Int                         -- memory type
data Memory = Memory (IOUArray Int Int)

type Code = Array Int Cmd

    

-- Opcodes    
data Cmd = Act  (MT -> MT -> MT)  Mem Mem 
         | Jmp  (MT -> MT -> Bool) Mem Mem String 
         | PJmp (MT -> MT -> Bool) Mem Mem Int 
         | Label String
         | IOout (MT -> IO ()) Mem
         | IOin  (IO (MT))     Mem
         | Push Mem
         | Pop Mem
         | Ret
         | Call String
         | PCall Int
         | Stop

data Mem = Sp
         | Ax
         | Bx
         | Cx
         | Dx
         | Ptr Mem Int
         | Ref Mem
         | Offset Int
         | Num Int
         | Nil
    deriving (Eq, Read, Show)



data MAsm a = MAsm (Assembler -> (a, Assembler))

instance Monad MAsm where
    (MAsm c1) >> (MAsm c2)      = MAsm (\asm -> let (_, asm') = c1 asm
                                                in c2 asm')

getAsm                          :: MAsm () -> Assembler
getAsm (MAsm masm)              = snd $ masm $ Assembler [] M.empty

mAsm                            :: (Assembler -> Assembler) -> MAsm ()
mAsm p                          = MAsm (\asm ->  ((), p asm))

-- Operations
mov                             :: Mem -> Mem -> MAsm ()
mov to from                     = mAsm (\(Assembler cmds dict) -> 
                                   Assembler 
                                    (cmds ++ [Act (\a b -> b) to from]) dict)

add                             :: Mem -> Mem -> MAsm ()
add to from                     = mAsm (\(Assembler cmds dict) -> 
                                   Assembler (cmds ++ [Act (+) to from]) dict)

dec                             :: Mem -> Mem -> MAsm ()
dec to from                     = mAsm (\(Assembler cmds dict) -> 
                                   Assembler (cmds ++ [Act (-) to from]) dict)

jmp                             :: String -> MAsm ()
jmp to                          = mAsm (\(Assembler cmds dict) -> 
                                   Assembler (cmds ++ [Jmp (==) Ax Ax to]) dict)

je                              :: Mem -> Mem -> String -> MAsm ()
je a b to                       = mAsm (\(Assembler cmds dict) -> 
                                   Assembler (cmds ++ [Jmp (==) a b to]) dict)

jne                             :: Mem -> Mem -> String -> MAsm ()
jne a b to                      = mAsm (\(Assembler cmds dict) -> 
                                   Assembler (cmds ++ [Jmp (/=) a b to]) dict)
                                   
jg                              :: Mem -> Mem -> String -> MAsm ()
jg a b to                       = mAsm (\(Assembler cmds dict) -> 
                                   Assembler (cmds ++ [Jmp (>) a b to]) dict) 

jge                             :: Mem -> Mem -> String -> MAsm ()
jge a b to                      = mAsm (\(Assembler cmds dict) -> 
                                   Assembler (cmds ++ [Jmp (>=) a b to]) dict)

jl                              :: Mem -> Mem -> String -> MAsm ()
jl a b to                       = mAsm (\(Assembler cmds dict) -> 
                                   Assembler (cmds ++ [Jmp (<) a b to]) dict)  

jle                             :: Mem -> Mem -> String -> MAsm ()
jle a b to                      = mAsm (\(Assembler cmds dict) -> 
                                   Assembler (cmds ++ [Jmp (<=) a b to]) dict)                                   
                                   
label                           :: String -> MAsm ()
label name                      = mAsm (\(Assembler cmds dict) -> 
                                   Assembler 
                                    cmds (M.insert name (length cmds) dict))

outI                            :: Mem -> MAsm ()
outI mem                        = mAsm (\(Assembler cmds dict) -> 
                                   Assembler 
                                    (cmds ++ [IOout (putStr . show) mem]) dict)

inI                             :: Mem -> MAsm ()
inI mem                         = mAsm (\(Assembler cmds dict) -> 
                                   Assembler 
                                    (cmds ++ [IOin (liftM read getLine) mem]) dict)

call                            ::String -> MAsm ()
call to                         = mAsm (\(Assembler cmds dict) -> 
                                   Assembler (cmds ++ [Call to]) dict) 

ret                             :: MAsm () 
ret                             = mAsm (\(Assembler cmds dict) -> 
                                   Assembler (cmds ++ [Ret]) dict) 
                                    
push                            :: Mem -> MAsm () 
push addr                       = mAsm (\(Assembler cmds dict) -> 
                                   Assembler (cmds ++ [Push addr]) dict) 

pop                             :: Mem -> MAsm () 
pop addr                        = mAsm (\(Assembler cmds dict) -> 
                                   Assembler (cmds ++ [Pop addr]) dict)                                   


-- Nothing or Just
jsj                             :: (Eq a) => Maybe a -> IO (Maybe b) -> IO (Maybe b)
jsj t f | t == Nothing          = return Nothing
        | otherwise             = f



-- Memory managment
memConst                        = 5

  -- memory get 
memGet 							:: Mem -> Memory -> IO (Maybe MT)
memGet Sp (Memory m)            = liftM Just $ readArray m 0
memGet Ax (Memory m)            = liftM Just $ readArray m 1
memGet Bx (Memory m)            = liftM Just $ readArray m 2
memGet Cx (Memory m)            = liftM Just $ readArray m 3
memGet Dx (Memory m)            = liftM Just $ readArray m 4

memGet (Offset x) (Memory m)    = do bnd <- getBounds m
                                     if x >= 0 && inRange bnd (x + memConst)
                                      then liftM Just $ readArray m (x + memConst)
                                      else return Nothing
                                  
memGet (Ptr p off) m'@(Memory m)
                                = do addr <- memGet p m'
                                     jsj addr $ 
                                       memGet (Offset ((fromJust addr) + off)) m'

memGet (Ref p) m                = memGet (Ptr p 0) m
memGet Nil _                    = return $ Just 0
memGet (Num x) _                = return $ Just x


  -- memory set
memSet                          :: Mem -> MT -> Memory -> IO (Maybe ())
memSet Sp v (Memory m)          = liftM Just $ writeArray m 0 v
memSet Ax v (Memory m)          = liftM Just $ writeArray m 1 v
memSet Bx v (Memory m)          = liftM Just $ writeArray m 2 v
memSet Cx v (Memory m)          = liftM Just $ writeArray m 3 v
memSet Dx v (Memory m)          = liftM Just $ writeArray m 4 v

memSet (Offset x) v (Memory m)  = do bnd <- getBounds m
                                     if x >= 0 && inRange bnd (x + memConst)
                                      then liftM Just $ writeArray m (x + memConst) v
                                      else return Nothing
                                  
memSet (Ptr p off) v m
                                = do addr <- memGet p m
                                     jsj addr $ 
                                       memSet (Offset ((fromJust addr) + off)) v m

memSet (Ref p) v m              = memSet (Ptr p 0) v m
memSet Nil _ _                  = return $ Just ()
memSet (Num _) _ _              = return $ Just ()



  -- memory update
memUpdate                       :: Mem -> (MT -> MT) -> Memory -> IO (Maybe ())
memUpdate p f m                 = do v <- memGet p m
                                     jsj v $ memSet p (f $ fromJust v) m


-- Is Nothing
isn                             :: (Eq a) => Int -> String -> IO (Maybe a) -> IO a
isn line msg v                  = v >>= (\v' ->  if v' == Nothing
                                                  then error ((show line) ++ ": " ++ msg)
                                                  else return $ fromJust v'
                                        )
{-
memDump                         :: Memory -> Int -> IO ()
memDump (Memory m) ip           = do
                                     l <- getElems m
                                     putStrLn $ show (ip, l)
-}

-- Interpreter
evaluate                        :: Code -> Memory -> IO ()
evaluate code mem
  = let eval                    :: Int -> Cmd -> IO (Int)
        eval ip (Act f to from) = do from' <- isn ip "Operand 'from': get error" $
                                                  memGet from mem
                                     to'   <- isn ip "Operand 'to': get error" $
                                                  memGet to mem 
                                     isn ip "Operand 'to': write error" $
                                         memSet to (f to' from') mem
                                     return (ip+1)

        eval ip (PJmp f a b to) 
                                = do a' <- isn ip "Operand 'a': get error" $
                                               memGet a mem
                                     b' <- isn ip "Operand 'b': get error" $
                                               memGet b mem 
                                     if f a' b'
                                      then return to
                                      else return (ip+1)

        eval ip (IOin f to)     = do v <- f
                                     isn ip "Operand: write error" $
                                         memSet to v mem
                                     return (ip+1)

        eval ip (IOout f from)  = do v <- isn ip "Operand: read error" $
                                              memGet from mem
                                     f v
                                     return (ip+1)
                                     
        eval ip (Push from)     = do v <- isn ip "Operand: read error" $
                                              memGet from mem
                                     isn ip "Stack write error" $
                                              memSet (Ref Sp) v mem
                                     isn ip "Sp update error" $
                                              memUpdate Sp ((-1) +) mem
                                     return (ip + 1)

        eval ip (Pop to)        = do isn ip "Sp update error" $
                                         memUpdate Sp (1 +) mem                                     
                                     v <- isn ip "Stack read error" $
                                              memGet (Ref Sp) mem
                                     isn ip "Operand: write error" $
                                         memSet to v mem
                                     return (ip + 1)

        eval ip (PCall to)      = do isn ip "Can't push to stack Ip" $
                                         memSet (Ref Sp) ip mem
                                     isn ip "Sp update error" $
                                              memUpdate Sp ((-1) +) mem
                                     return to

        eval ip Ret             = do isn ip "Sp update error" $
                                         memUpdate Sp (1 +) mem 
                                     v <- isn ip "Stack read error" $
                                              memGet (Ref Sp) mem
                                     return (v + 1)

                                     
        loop                    :: Int -> IO ()
        loop ip                 = if inRange (A.bounds code) ip
                                   then do ip' <- eval ip (code A.! ip)
                                           -- memDump mem ip
                                           loop ip'
                                           
                                   else return ()
     in loop 0





asmToCode                       :: Assembler -> Assembler
asmToCode (Assembler ((Call name):as) dict)
                                = case (M.lookup name dict) of
                                    Nothing  -> error ("Call destination unresolved: " ++ name)
                                    (Just i) -> (Assembler
                                                 ((PCall i):(cmds $ asmToCode (Assembler as dict)))
                                                 dict)
asmToCode (Assembler ((Jmp f a b name):as) dict)
                                = case (M.lookup name dict) of
                                    Nothing  -> error ("Call destination unresolved: " ++ name)
                                    (Just i) -> (Assembler
                                                 ((PJmp f a b i):(cmds $ asmToCode (Assembler as dict)))
                                                 dict)
asmToCode (Assembler (a:as) dict)  
                                = (Assembler (a:(cmds $ asmToCode (Assembler as dict))) dict)

asmToCode (Assembler [] dict)      = Assembler [] dict


-- Execute

boot :: Int -> MAsm () -> MAsm ()
boot size client
  = do mov Sp $ Num size
       client


execute :: Int -> MAsm () -> IO ()
execute size masm
  = do let asm = getAsm (boot size masm)
           (Assembler asm' _) = asmToCode asm
           codeArray = A.listArray (0, -1 + length asm') asm'
       mem <- newArray (0, memConst + size) 0 :: IO (IOUArray Int Int)
       evaluate codeArray (Memory mem)
       return ()  















