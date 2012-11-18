import Text.Printf
import System.Environment
import System.Exit
import Control.Exception    as E

main :: IO ()
main = flip E.catch hdl $
     do [w] <- getArgs
        print_primes (read w)
      where
        hdl :: SomeException -> IO ()
        hdl _ =
            do putStrLn "uage: primes <num-primes>"
               exitWith (ExitFailure 1)

print_primes :: Int -> IO ()
print_primes sz = mapM_ prt (take sz (zip [1..] primes))
      where
        prt :: (Int,Integer) -> IO ()
        prt (i,p) = putStr (printf "prime %3d: %d\n" i p)
	  
primes :: [Integer]
primes = sieve [2..]
      where
        sieve (p:xs) = p : sieve [ x | x<-xs, x `mod` p /= 0]
