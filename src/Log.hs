module Log where

import System.IO.Unsafe (unsafePerformIO)
import System.IO        (putStrLn)

slog :: (Show a) => String -> a -> a
slog s var = unsafePerformIO $ do
    putStrLn $ s ++ ": " ++ show var
    return var

nlog :: String -> a -> a
nlog s var = unsafePerformIO $ do
    putStrLn s
    return var

vlog :: (Show a) => a -> a
vlog var = unsafePerformIO $ do
    print  var
    return var