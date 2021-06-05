module Inigo.Async.Promise

export
data Promise : Type -> Type where
  MkPromise : ((a -> IO ()) -> (String -> IO ()) -> IO ()) -> Promise a

export
Functor Promise where
  map f (MkPromise cmd) = MkPromise (\succ => \err => cmd (\x => succ (f x)) err)

mutual
  export
  Applicative Promise where
    pure x = MkPromise (\succ => \err => succ x)
    x <*> y = x >>= (\f => f <$> y)

  export
  Monad Promise where
    (MkPromise cmd) >>= f = MkPromise (\succ =>
                                        \err =>
                                                cmd (\x =>
                                                          let (MkPromise cmd_) = (f x)
                                                          in cmd_ succ err
                                                    ) err
                                      )

export
resolve : Promise a -> (a -> IO ()) -> (String -> IO ()) -> IO ()
resolve (MkPromise cmd) ok err =
  cmd ok err

export
run : Promise a -> IO ()
run p =
  resolve p (\_ => pure ()) (\err => putStrLn ("Error: " ++ err))

-- I can fold these, but that's a bit of an issue since
-- they will end up running sequentially, which is really
-- not the intent here, but for now...
export
all : List (Promise a) -> Promise (List a)
all promises =
  doAll promises
  where
    doAll : List (Promise a) -> Promise (List a)
    doAll (p :: ps) =
      do
        x <- p
        rest <- doAll ps
        pure (x :: rest)
    doAll [] = pure []

export
lift : a -> Promise a
lift x = MkPromise (\ok => \err => ok x)

export
liftIO : IO a -> Promise a
liftIO x = MkPromise (\ok => \err => x >>= ok)

export
parallel : Promise a -> Promise a -> Promise a
parallel (MkPromise s1) (MkPromise s2) = MkPromise $ \err => \cb => do
  s1 err cb
  s2 err cb

public export
promise : Type -> Type
promise a = (a -> IO ()) -> (String -> IO ()) -> PrimIO ()

export
promisify : promise a -> Promise a
promisify prim =
  MkPromise (\ok, err => primIO $ prim ok err)

export
boolToInt : Bool -> Int
boolToInt False = 0
boolToInt True = 1