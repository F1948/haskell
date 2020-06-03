module Monad where
-- stp 5.2


data Log a = Log [String] a deriving Show

fucking_log :: a -> (a -> Log b) -> (String,b)
fucking_log x f = (a, b) where Log [a] b = f x

returnLog :: a -> Log a
returnLog = Log []

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f msg = (\x -> Log [msg] (f x) )

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers x f g = let(msg1, res1) = fucking_log x f
                       (msg2, res2) = fucking_log res1 g
                    in Log (msg1 : msg2 : []) res2

bindLog :: Log a -> (a -> Log b) -> Log b
bindLog a f = case a of Log s v -> case fucking_log v f of (n,c) -> Log (s++[n]) c


execLoggersList :: a -> [a -> Log a] -> Log a
execLoggersList a f = foldl bindLog (returnLog a) f


t001 = execLoggersList 3 [add1Log, mult2Log, \x -> Log ["multiplied by 100"] (x * 100)]
t000 = returnLog 3 `bindLog` add1Log `bindLog` mult2Log `bindLog` (\x -> Log ["multiplied by 100"] (x * 100))
--Log ["added one","multiplied by 2","multiplied by 100"] 800

t0 = Log ["nothing done yet"] 0 `bindLog` add1Log
--Log ["nothing done yet","added one"] 1

t00 = Log ["nothing done yet"] 3 `bindLog` add1Log `bindLog` mult2Log
--Log ["nothing done yet","added one","multiplied by 2"] 8

add1Log = toLogger (+1) "added one"
t1 = add1Log 3
-- Log ["added one"] 4

mult2Log = toLogger (* 2) "multiplied by 2"
t2 =  mult2Log 3
-- Log ["multiplied by 2"] 6

t3 = execLoggers 3 add1Log mult2Log
--Log ["added one","multiplied by 2"] 8





