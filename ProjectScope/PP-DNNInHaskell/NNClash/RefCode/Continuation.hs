--  Code originally found here∷
-- http://www.haskellforall.com/2014/04/how-continuation-monad-works.html

import           Control.Applicative

newtype ContT x m r = ContT { (>>-) :: (r -> m x) -> m x }

instance Functor (ContT x m) where
    fmap f m  = ContT $ \_return ->  -- fmap f m =
        m >>- \a ->                  --     m >>= \a ->
        _return (f a)                --     return (f a)

instance Applicative (ContT x m) where
    pure r    = ContT $ \_return ->  -- pure r =
        _return r                    --     return r

    mf <*> mx = ContT $ \_return ->  -- mf <*> mx =
        mf >>- \f ->                 --     mf >>= \f ->
        mx >>- \x ->                 --     mx >>= \x ->
        _return (f x)                --     return (f x)

instance Monad (ContT x m) where
    return r  = ContT $ \_return ->  -- return r =
        _return r                    --     return r

    m >>= f   = ContT $ \_return ->  -- m >>= f =
        m   >>- \a ->                --     m   >>= \a ->
        f a >>- \b ->                --     f a >>= \b ->
        _return b                    --     return b
