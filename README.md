# backtrack
### Racket-style pattern matching failure continuation for Haskell

Haskell allows guards in pattern matches like the following:

```haskell
case e of
  Plus (Const n) e2 | n > 0 -> [...]
  [...]
```

where ``Plus (Const n) e2`` only matches if ``n`` is positive. If the guard fails, the remaining patterns are tried.
But these guards have to be pure expressions.
Racket, on the other hand, exposes a continuation that allows for general computation to happen to decide, if the pattern matches:

```racket
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
````

Using ``#?`` we can implement the same in Haskell:

```haskell
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
```

But unlike regular guards, ``#?`` allows for arbitrary monadic effects:

```haskell
example :: IO ()
example = do
  x <-
    $((#?)
      [| case () of
            () -> \k -> do
              liftIO $ putStrLn "Do you want to match the first case?"
              liftIO getLine >>= \case
                "yes" -> return "First case"
                "no" -> k
            () -> \k -> do
              liftIO $ putStrLn "Do you want to match the second case?"
              liftIO getLine >>= \case
                "yes" -> return "Third case"
                "no" -> k
        |])
  putStrLn x
```

