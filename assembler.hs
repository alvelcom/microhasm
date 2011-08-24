module Assembler where


import qualified Data.Map as M
import Data.Maybe
import Control.Monad


-- Base types
data Assembler = Assembler [Cmd] (M.Map String Int)
    deriving (Show)

data Memory = Memory [Int] Int
    deriving (Eq, Read, Show)
    

-- Opcodes    
data Cmd = Act (Int -> Int -> Int)  Mem Mem 
         | Jmp (Int -> Int -> Bool) Mem Mem String 
         | Label String
         | IOout (Int -> IO ()) Mem
         | IOin  (IO (Int))     Mem
         | Push Mem
         | Pop Mem
         | Ret
         | Call String
         | Stop
    deriving (Show)

data Mem = Ax
         | Bx
         | Cx
         | Dx
         | Sp
         | Ptr Mem
         | Aptr Mem Int
         | Offset Int
         | Num Int
         | Nil
    deriving (Eq, Read, Show)



instance Show (Int -> Int -> Int) where
    show _ = "iii"
instance Show (Int -> Int -> Bool) where
    show _ = "iib"
instance Show (IO Int) where
    show _ = "(IO Int)"
instance Show (Int -> IO ()) where
    show _ = "(IO Int -> ())"

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

-- Memory managment
memConst                        = 5

memGet                          :: Mem -> Memory -> Maybe Int
memGet Ax (Memory m _)          = Just $ m !! 1
memGet Bx (Memory m _)          = Just $ m !! 2
memGet Cx (Memory m _)          = Just $ m !! 3
memGet Dx (Memory m _)          = Just $ m !! 4  
memGet Sp (Memory m _)          = Just $ m !! 5 
memGet (Offset addr) 
       (Memory m size)          = if (addr <= (size + memConst) && addr > memConst)
                                  then Just $ m !! (addr + memConst)
                                  else Nothing

memGet (Ptr addr)
       memory@(Memory m size)   = case (memGet addr memory) of
                                    Nothing -> Nothing
                                    Just addr' | addr' > memConst && addr' <= (size + memConst)
                                                 -> Just $ m !! (addr' + memConst)
                                               | otherwise 
                                                 -> Nothing
memGet (Aptr addr off)
       memory@(Memory m size)   = case (memGet addr memory) of
                                    Nothing -> Nothing
                                    Just addr' | (addr' + off) > memConst && (addr' + off) <= (size + memConst)
                                                 -> Just $ m !! (addr' + memConst + off)
                                               | otherwise 
                                                 -> Nothing                                                 
                                  
memGet (Num x) _                = Just x
memGet Nil _                    = Just 0



memUpdate'                      :: Maybe Int -> (Int -> Int) -> Memory -> Maybe Memory
memUpdate' Nothing _ _          = Nothing
memUpdate' (Just x)
           f
           (Memory m s) | (x > 0 && x <= (s + memConst)) 
                                = let (begin, end) = splitAt x m
                                  in Just $ Memory (begin ++ [f $ head end] ++ (tail end)) s
                                                           
                        | otherwise
                                = Nothing

memUpdate                       :: Mem -> (Int -> Int) -> Memory -> Maybe Memory
memUpdate Ax v m                = memUpdate' (Just 1) v m
memUpdate Bx v m                = memUpdate' (Just 2) v m
memUpdate Cx v m                = memUpdate' (Just 3) v m
memUpdate Dx v m                = memUpdate' (Just 4) v m
memUpdate Sp v m                = memUpdate' (Just 5) v m
memUpdate addr@(Offset _)
          val
          mem                   = let addr' = memGet addr mem 
                                   in if addr' == Nothing
                                      then Nothing
                                      else memUpdate' (Just ((memConst) + (fromJust addr'))) val mem

memUpdate (Aptr addr off)
          val
          mem                   = let addr' = memGet addr mem 
                                   in if addr' == Nothing
                                      then Nothing
                                      else memUpdate' (Just ((memConst) + off + (fromJust addr'))) val mem
memUpdate (Ptr addr)
          val
          mem                   = let addr' = memGet addr mem 
                                   in if addr' == Nothing
                                      then Nothing
                                      else memUpdate' (Just ((memConst) + (fromJust addr'))) val mem
         
memUpdate (Num _) _ _           = Nothing
memUpdate Nil _ _               = Nothing


memSet                          :: Mem -> Int -> Memory -> Maybe Memory
memSet p val                    = memUpdate p (\ _ -> val)


memAlloc                        :: Int -> Memory
memAlloc size                   = Memory (take (size + memConst) (repeat 0) {-[1..] -}) size

-- Interpreter

eval                           :: Assembler -> Memory -> Int -> IO Memory
eval asm@(Assembler cmd dict) mem eip
  = if (eip >= length cmd) || (eip < 0)
    then return mem
    else do (eip', mem') <- runCmd mem (cmd !! eip) eip dict
            --putStrLn $ show (eip, eip', mem')
            eval asm mem' eip'
    

runCmd                         :: Memory -> Cmd -> Int -> M.Map String Int -> IO (Int, Memory)
runCmd mem (Act op to from) eip _
      = if (to' == Nothing) || (from' == Nothing)
        then error "ACT: Memory read error"
        else let c    = op (fromJust to') (fromJust from')
                 mem' = memSet to c mem
             in if mem' == Nothing
                then error "Memory write error"
                else return (eip + 1, fromJust mem')
        where            
          to'   = memGet to mem
          from' = memGet from mem

runCmd mem (Jmp bool a b name) eip dict
      = if (a' == Nothing) || (b' == Nothing)
        then error "JUMP: Memory read error"
        else if bool (fromJust a') (fromJust b')
             then case (M.lookup name dict) of
                    Nothing -> error "Invalid label for jump"
                    Just i -> return (i, mem)
             else return (eip + 1, mem)
        where            
          a' = memGet a mem
          b' = memGet b mem          

runCmd mem (IOout op from) eip _
      = if from' == Nothing
        then error "IO: Memory read error"
        else do op (fromJust from')
                return (eip + 1, mem)
        where            
          from' = memGet from mem

runCmd mem (IOin op to) eip _
      = if to' == Nothing
        then error "Memory read error"
        else do c        <- op 
                let mem' =  memSet to c mem
                if mem' == Nothing
                  then error "Memory write error"
                  else return (eip + 1, fromJust mem')
        where            
          to'   = memGet to mem

runCmd mem (Call name) eip dict
      = case (M.lookup name dict) of
          Nothing -> error "Invalid label for call"
          Just i -> do let mem' = memSet (Ptr Sp) (eip + 1) $ fromJust $ memUpdate Sp ((-1)+) mem
                        in if mem' == Nothing 
                            then error "Stack is overflow"
                            else return (i, fromJust mem')

runCmd mem Ret eip dict
      = do let eip' = memGet (Ptr Sp) mem
            in if eip' == Nothing
                then error "Bad return address"
                else return (fromJust eip', fromJust $ memUpdate Sp (1+) mem)

           
runCmd mem (Pop addr) eip _
      = let rsp  = memGet (Ptr Sp) mem
            mem' = memSet addr (fromJust rsp) mem
        in if rsp == Nothing ||  mem' == Nothing 
           then error "Memory write error in POP operation"
           else return (eip + 1, fromJust $ memUpdate Sp (1+) $ fromJust mem')

runCmd mem (Push addr) eip _
      = let mem' = fromJust $ memUpdate Sp ((-1)+) mem
            val = memGet addr mem'
            mem'' = memSet (Ptr Sp) (fromJust val) mem'
        in if val == Nothing || mem'' == Nothing
           then error "Memory read error in PUSH operation"
           else return (eip + 1, fromJust mem'')
   
runCmd mem Stop _ _ = return (-1, mem)


-- Execute

execute :: Int -> MAsm () -> IO ()
execute size masm = do eval (getAsm masm) (memAlloc size) 0
                       return ()  















