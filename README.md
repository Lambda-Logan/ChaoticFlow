# ChaoticFlow

This is an encoding of 'flow' from fluid dynamics as a monoidal functor from a monoid `t` to `Endo a`.

According to the definition on wikipedia, a flow is a mapping from addition on the reals to a function on `a`. We generalize addition on the reals to a monoid and encode it as `newtype Flow' s t a = Flow{ unFlow :: Kleisli (State s) t a }`

A `Flow` is implements a wide range of categorical operations:

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

https://en.wikipedia.org/wiki/Flow_(mathematics)

![Image of Definition of Flow](https://raw.githubusercontent.com/Lambda-Logan/ChaoticFlow/main/Flow%20(mathematics)%20-%20Wikipedia.png)
