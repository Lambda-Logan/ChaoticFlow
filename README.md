# ChaoticFlow
This is an encoding of 'flow' from fluid dynamics as a monoidal functor from a monoid `t` to the monoid `Endo a`.

According to the definition on wikipedia, a flow is a mapping from addition on the reals to a set of functions `a->a`. 

https://en.wikipedia.org/wiki/Flow_(mathematics)

![Image of Definition of Flow](https://raw.githubusercontent.com/Lambda-Logan/ChaoticFlow/main/Flow%20(mathematics)%20-%20Wikipedia.png)


We generalize addition on the reals to a monoid and encode it as `newtype Flow' s t a = Flow{ unFlow :: Kleisli (State s) t a }`. That might seem like a weird type for it, but it can be arrived at by the following:
```haskell
-- for t = Reals
(s, t) -> s -- see wiki
t -> s -> s
t -> s -> (a, s) -- needs output parameter to fmap
t -> State s a
Kleisli (State s) t a
```
Which makes sense as a mapping from time `t` to a stateful effect `State s a`.


# A `Flow` correctly admits a wide range of categorical operations:

```haskell
deriving instance Functor (Flow' s t)
deriving instance Applicative (Flow' s t)
deriving instance Monad (Flow' s t)
deriving instance Category (Flow' s)
deriving instance Arrow (Flow' s)
deriving instance ArrowApply (Flow' s)
deriving instance ArrowChoice (Flow' s)
deriving instance Profunctor (Flow' s)


type Flow t a = (Flow' a t a)

instance (Semigroup t) => Semigroup (Flow t a)...

instance (Monoid t) => Monoid (Flow t a)...
```


--------------------------------------------------

## Requires ghc 8.10.1+
(`Kleisli` oddly didn't have a `Profunctor` instance before then)
