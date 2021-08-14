Name TBD
========

NB: This is a literate Haskell file, so all the code here is compilable.
Let's get some pragmas and imports out of the way to make that possible:

> {-# LANGUAGE FlexibleInstances, DataKinds, GeneralizedNewtypeDeriving,
> TypeApplications, TypeOperators #-}
> import RIO
> import Has
> import Data.Tagged

Motivation
----------

I recently tried out [RIO](https://hackage.haskell.org/package/rio-0.1.20.0 )
with a project of mine. RIO advocates using a lot of `Has`-style classes. A
quick example:

> data Config = Config { verbose :: Bool, counterIncrement :: Int }
> class HasConfig env where
>   configL :: Lens' env Config
> instance HasConfig Config where
>   configL = id
>
> newtype Counter = Counter Int deriving Num
> class HasCounter env where
>   counterL :: Lens' env (IORef Counter)
> instance HasCounter (IORef Counter) where
>  counterL = id
>
> data App = App { appConfig :: Config, appCounter :: IORef Counter }
> class (HasConfig env, HasCounter env) => HasApp env where
>   appL :: Lens' env App
> instance HasApp App where
>   appL = id
> instance HasConfig App where
>   configL = lens appConfig (\x y -> x {appConfig = y})
> instance HasCounter App where
>   counterL = lens appCounter (\x y -> x {appCounter = y})

Note in particular how `HasApp` has `HasConfig` and `HasCounter` as
superclasses. With this, we can define a function like

> countOneStep :: (HasConfig env, HasCounter env) => RIO env ()
> countOneStep = do
>   increment <- counterIncrement <$> view configL
>   flip modifyIORef' (+ Counter increment) =<< view counterL

which requests the capabilities it requires in its function signature. Using
superclasses means we can call this function in any environment that provides
all of these capabilities - like `App`:

> countABunch :: HasApp env => RIO env ()
> countABunch = do
>   countOneStep
>   countOneStep
>   countOneStep

We can add capabilities to an environment with `mapRIO`:

> caller :: HasConfig env => RIO env ()
> caller = do
>   counter <- newIORef 0
>   mapRIO (\env -> App (env^.configL) counter) countABunch

This seems very nice, and I recently wrote [some template haskell] to
(https://gist.github.com/JakobBruenker/57561e42da3e7220498013b7cf9f4120) that
does something very similar, to eliminate the boilerplate. In general this is
quite pleasant, but after using it for a bit, one thing that I wish I were able
to do was adding capabilities without having to make a new dedicated type.
That's generally not a huge burden, but it's not really worth doing if you just
need it for one or two function calls. In those cases, unless you have a
lightweight way of adding capabilities, you're better off just passing the
additional capability as a regular argument.

After thinking about it for a bit and some trial and error, this is the kind of
thing I'd like to be able to write:

> -- Using type synonyms instead of subclasses
> type Config' = Tagged "verbose" Bool >< Tagged "counter increment" Int
> type App' = Config' >< Counter
>
> countOneStep' :: Has (Counter >< Tagged "counter increment" Int) env => RIO env ()
> countOneStep' = do
>   increment <- view (the @"counter increment")
>   flip modifyIORef' (+ increment) =<< view (the @Counter)
>
> countABunch' :: Has App' env => RIO env ()
> countABunch' = do
>   countOneStep'
>   countOneStep'
>   countOneStep'
>
> caller' :: Has Config' env => RIO env ()
> caller' = do
>   counter' <- newIORef 0
>   withCap counter countABunch'

This gets us squarely into the realm of extensible records, and there are other
packages that do somewhat comparable things, among them
[vinyl](https://hackage.haskell.org/package/vinyl) and
[data-has](https://hackage.haskell.org/package/data-has).

Still, in implementing it myself I was able to get exactly the interface I
wanted, and I learned a lot in the process, some of which I thought might be of
interest to others.

Though, if I'm being honest, in production code I would probably still prefer
something like the template haskell solution I linked above. `Has` leads to
error messages that are somewhat harder to understand, and lookup time for a
lens increases linearly with the amount of capabilities that are provided.

Nevertheless, over the next few blog posts, I will explain how I implemented
the TBD package, which provides the above interface.
