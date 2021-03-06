# backtrack
### Racket-style pattern matching failure continuation for Haskell

Haskell allows guards in pattern matches like the following:

```haskell
case e of
  Div e (Const n) | n <> 0 -> [...]
  [...]
```

where ``Div e (Const n)`` only matches if ``n`` is non-zero. If the guard fails, the remaining patterns are tried.
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

Using ``#?`` from this library we can implement the same in Haskell using ``TemplateHaskell``:

```haskell
m :: (Num a, Monad m, Eq a) => [a] -> m String
m x =
  $((#?)
     [| case x of
         [a, b, c] -> \exit -> f x exit
         [a, b, c] -> \_ -> return "sum is not six"
      |])

f :: (Eq a, Num a, Monad m) => [a] -> m String -> m String
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
                "yes" -> return "Second case"
                "no" -> k
        |])
  putStrLn x
```

