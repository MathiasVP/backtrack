{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
import Backtrack
import Control.Monad.Cont
import Control.Monad.State
import Control.Lens

{-
> (define (m x)
    (match x
      [(list a b c)
       (=> exit)
       (f x exit)]
      [(list a b c) 'sum-is-not-six]))
> (define (f x exit)
    (if (= 6 (apply + x))
        'sum-is-six
        (exit)))
> (m '(1 2 3))
'sum-is-six

> (m '(2 3 4))
'sum-is-not-six
-}

m :: (Num a, Monad m, Eq a) => [a] -> m [Char]
m x =
  $((#?)
     [| case x of
         [a, b, c] -> \exit -> f x exit
         [a, b, c] -> \_ -> return "sum is not six"
      |])

f :: (Eq a, Num a, Monad m) => [a] -> m [Char] -> m [Char]
f x exit =
  if 6 == sum x then return "sum is six"
  else exit

case2 :: MonadIO m => m String -> m String
case2 k = do
  liftIO $ putStrLn "Matched the second case. Do you want to hit this one?"
  liftIO getLine >>= \case
    "yes" -> return "Second case"
    "no" -> k

example2 = do
  x <-
    $((#?)
      [| case () of
            () -> \k -> do
              liftIO $ putStrLn "Matched the first case. Do you want to hit this one?"
              liftIO getLine >>= \case
                "yes" -> return "First case"
                "no" -> k
            () -> case2
            () -> \k -> do
              liftIO $ putStrLn "Matched the third case. Do you want to hit this one?"
              liftIO getLine >>= \case
                "yes" -> return "Third case"
                "no" -> k
        |])
  putStrLn x
