import Euler
import System.Environment

main = do args <- getArgs
          solve args

solve []                   = return ()
solve ("-solution1":args)  = do print solution1;     solve args
solve ("-solution2":args)  = do print solution2;     solve args
solve ("-solution3":args)  = do print solution3;     solve args
solve ("-solution4":args)  = do print solution4;     solve args
solve ("-solution5":args)  = do print solution5;     solve args
solve ("-solution6":args)  = do print solution6;     solve args
solve ("-solution7":args)  = do print solution7;     solve args
solve ("-solution8":args)  = do print solution8;     solve args
solve ("-solution9":args)  = do print solution9;     solve args
solve ("-solution10":args) = do print solution10;    solve args
solve ("-solution11":args) = do print solution11;    solve args
solve ("-solution12":args) = do print solution12;    solve args
solve ("-solution13":args) = do putStrLn solution13; solve args
solve ("-solution14":args) = do print solution14;    solve args
solve ("-solution15":args) = do print solution15;    solve args
solve ("-solution16":args) = do print solution16;    solve args
solve ("-solution17":args) = do print solution17;    solve args
solve ("-solution18":args) = do print solution18;    solve args
solve ("-solution19":args) = do print solution19;    solve args
solve ("-solution20":args) = do print solution20;    solve args
solve ("-solution21":args) = do print solution21;    solve args
solve ("-solution22":args) = do solution22;          solve args
solve ("-solution23":args) = do print solution23;    solve args
solve ("-solution24":args) = do print solution24;    solve args
solve ("-solution25":args) = do print solution25;    solve args
solve _                    = print "usage: ./euler -solution<num>\n"
