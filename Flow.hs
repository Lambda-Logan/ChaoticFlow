{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, DeriveFunctor #-}
import Prelude hiding ((.), id)
import Data.Monoid
import Data.Group (Group, invert)
import Control.Category 
import Control.Arrow --(Kleisli)
import Data.Profunctor
import Data.Traversable (mapAccumR, mapAccumL)
import Control.Monad.State.Strict (State, get, put, MonadState, state, evalState)


newtype Flow' s t a = Flow{ unFlow :: Kleisli (State s) t a }

deriving instance Functor (Flow' s t)
deriving instance Applicative (Flow' s t)
deriving instance Monad (Flow' s t)

--deriving instance MonadPlus (Flow' s' m)
deriving instance Category (Flow' s)
deriving instance Arrow (Flow' s)
deriving instance ArrowApply (Flow' s)
deriving instance ArrowChoice (Flow' s)
deriving instance Profunctor (Flow' s)
--deriving instance ArrowPlus (Flow' s')
--deriving instance ArrowZero (Flow' s')
--deriving instance Generic1 (Flow' s' m s)

type Flow t a = (Flow' a t a)

runFlow :: Flow' s t a -> t -> s -> a
runFlow (Flow (Kleisli f)) = evalState . f

fnToFlow :: (t -> s -> a) -> Flow' s t a
fnToFlow f = Flow $ Kleisli $ \t -> state $ \s -> (f t s, s)

stateToFlow :: (t -> State s a) -> Flow' s t a
stateToFlow = Flow . Kleisli

timeDelta :: Flow' s t t
timeDelta = stateToFlow $ \t -> return t   

instance (Semigroup t) => Semigroup (Flow t a) where
    a <> b = fnToFlow $ \m -> runFlow a m . runFlow b m
    --MUST HOLD: Flow a (m <> m') == (Flow a m)  . (Flow a m')

instance (Monoid t) => Monoid (Flow t a) where
    mempty = fnToFlow $ const id
    --MUST HOLD for Flow: forall . Monoid t => (Flow t a) mempty == id

instance (Group t)=> Group (Flow t a) where
    invert = lmap invert

instance MonadState s (Flow' s t) where
    get = stateToFlow $ const get
    put = stateToFlow . const . put

toEndo :: Flow t a -> t -> Endo a
toEndo f = Endo . runFlow f

makeStep :: (Monoid t) => Flow t s -> (t, s) -> t -> ((t, s), s)
makeStep f (m, s) m' = ((m'', s'), s')
                        where
                          m'' = m <> m'
                          s' = runFlow f m'' s

timeSeriesR :: (Traversable t , Monoid m) => Flow m s -> s -> t m -> t s
timeSeriesR f s = snd . mapAccumR (makeStep f) (mempty, s) 

timeSeries  :: (Traversable t , Monoid m) => Flow m s -> s -> t m -> t s
timeSeries  f s = snd . mapAccumL (makeStep f) (mempty, s) 





timeIncs :: Int -> Float -> [Sum Float]
timeIncs i = replicate i . Sum






liss :: Float -> Float -> (Sum Float -> (Float, Float))
liss a b (Sum t) = (sin $ a * t, sin $ b * t)
--https://www.desmos.com/calculator



main = do
        print $ timeSeries (arr $ liss 2 3) (0.0,0.0) $ timeIncs 200 0.4
