{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE AutoDeriveTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Writer.Lazy
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  R.Paterson@city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- The lazy 'WriterT2' monad transformer, which adds collection of
-- outputs (such as a count or string output) to a given monad.
--
-- This monad transformer provides only limited access to the output
-- during the computation.  For more general access, use
-- "Control.Monad.Trans.State" instead.
--
-- This version builds its output lazily; for a strict version with
-- the same interface, see "Control.Monad.Trans.Writer.Strict".
-----------------------------------------------------------------------------

module WriterCopy (
    -- * The Writer monad
    WriterCopy,
    writer,
    runWriter,
    execWriter,
    mapWriter,
    -- * The WriterT2 monad transformer
    WriterT2(..),
    execWriterT2,
    mapWriterT2,
    -- * Writer operations
    tell,
    listen,
    listens,
    pass,
    censor,
    -- * Lifting other operations
    liftCallCC,
    liftCatch,
  ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Functor.Classes
import Data.Functor.Identity

import Control.Applicative
import Control.Monad
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.Signatures
import Control.Monad.Zip (MonadZip(mzipWith))
import Data.Foldable
import Data.Monoid
import Data.Traversable (Traversable(traverse))
import Prelude hiding (null, length)

-- ---------------------------------------------------------------------------
-- | A writer monad parameterized by the type @w@ of output to accumulate.
--
-- The 'return' function produces the output 'mempty', while @>>=@
-- combines the outputs of the subcomputations using 'mappend'.
type WriterCopy w = WriterT2 w Identity

-- | Construct a writer computation from a (result, output) pair.
-- (The inverse of 'runWriter'.)
writer :: (Monad m) => (a, w) -> WriterT2 w m a
writer = WriterT2 . return
{-# INLINE writer #-}

-- | Unwrap a writer computation as a (result, output) pair.
-- (The inverse of 'writer'.)
runWriter :: WriterCopy w a -> (a, w)
runWriter = runIdentity . runWriterT2
{-# INLINE runWriter #-}

-- | Extract the output from a writer computation.
--
-- * @'execWriter' m = 'snd' ('runWriter' m)@
execWriter :: WriterCopy w a -> w
execWriter m = snd (runWriter m)
{-# INLINE execWriter #-}

-- | Map both the return value and output of a computation using
-- the given function.
--
-- * @'runWriter' ('mapWriter' f m) = f ('runWriter' m)@
mapWriter :: ((a, w) -> (b, w')) -> WriterCopy w a -> WriterCopy w' b
mapWriter f = mapWriterT2 (Identity . f . runIdentity)
{-# INLINE mapWriter #-}

-- ---------------------------------------------------------------------------
-- | A writer monad parameterized by:
--
--   * @w@ - the output to accumulate.
--
--   * @m@ - The inner monad.
--
-- The 'return' function produces the output 'mempty', while @>>=@
-- combines the outputs of the subcomputations using 'mappend'.
newtype WriterT2 w m a = WriterT2 { runWriterT2 :: m (a, w) }

instance (Eq w, Eq1 m) => Eq1 (WriterT2 w m) where
    liftEq eq (WriterT2 m1) (WriterT2 m2) = liftEq (liftEq2 eq (==)) m1 m2
    {-# INLINE liftEq #-}

instance (Ord w, Ord1 m) => Ord1 (WriterT2 w m) where
    liftCompare comp (WriterT2 m1) (WriterT2 m2) =
        liftCompare (liftCompare2 comp compare) m1 m2
    {-# INLINE liftCompare #-}

instance (Read w, Read1 m) => Read1 (WriterT2 w m) where
    liftReadsPrec rp rl = readsData $
        readsUnaryWith (liftReadsPrec rp' rl') "WriterT2" WriterT2
      where
        rp' = liftReadsPrec2 rp rl readsPrec readList
        rl' = liftReadList2 rp rl readsPrec readList

instance (Show w, Show1 m) => Show1 (WriterT2 w m) where
    liftShowsPrec sp sl d (WriterT2 m) =
        showsUnaryWith (liftShowsPrec sp' sl') "WriterT2" d m
      where
        sp' = liftShowsPrec2 sp sl showsPrec showList
        sl' = liftShowList2 sp sl showsPrec showList

instance (Eq w, Eq1 m, Eq a) => Eq (WriterT2 w m a) where (==) = eq1
instance (Ord w, Ord1 m, Ord a) => Ord (WriterT2 w m a) where compare = compare1
instance (Read w, Read1 m, Read a) => Read (WriterT2 w m a) where
    readsPrec = readsPrec1
instance (Show w, Show1 m, Show a) => Show (WriterT2 w m a) where
    showsPrec = showsPrec1

-- | Extract the output from a writer computation.
--
-- * @'execWriterT2' m = 'liftM' 'snd' ('runWriterT2' m)@
execWriterT2 :: (Monad m) => WriterT2 w m a -> m w
execWriterT2 m = do
    ~(_, w) <- runWriterT2 m
    return w
{-# INLINE execWriterT2 #-}

-- | Map both the return value and output of a computation using
-- the given function.
--
-- * @'runWriterT2' ('mapWriterT2' f m) = f ('runWriterT2' m)@
mapWriterT2 :: (m (a, w) -> n (b, w')) -> WriterT2 w m a -> WriterT2 w' n b
mapWriterT2 f m = WriterT2 $ f (runWriterT2 m)
{-# INLINE mapWriterT2 #-}

instance (Functor m) => Functor (WriterT2 w m) where
    fmap f = mapWriterT2 $ fmap $ \ ~(a, w) -> (f a, w)
    {-# INLINE fmap #-}

instance (Foldable f) => Foldable (WriterT2 w f) where
    foldMap f = foldMap (f . fst) . runWriterT2
    {-# INLINE foldMap #-}
    null (WriterT2 t) = null t
    length (WriterT2 t) = length t

instance (Traversable f) => Traversable (WriterT2 w f) where
    traverse f = fmap WriterT2 . traverse f' . runWriterT2 where
       f' (a, b) = fmap (\ c -> (c, b)) (f a)
    {-# INLINE traverse #-}

instance (Monoid w, Applicative m) => Applicative (WriterT2 w m) where
    pure a  = WriterT2 $ pure (a, mempty)
    {-# INLINE pure #-}
    f <*> v = WriterT2 $ liftA2 k (runWriterT2 f) (runWriterT2 v)
      where k ~(a, w) ~(b, w') = (a b, w `mappend` w')
    {-# INLINE (<*>) #-}

instance (Monoid w, Alternative m) => Alternative (WriterT2 w m) where
    empty   = WriterT2 empty
    {-# INLINE empty #-}
    m <|> n = WriterT2 $ runWriterT2 m <|> runWriterT2 n
    {-# INLINE (<|>) #-}

instance (Monoid w, Monad m) => Monad (WriterT2 w m) where
    m >>= k  = WriterT2 $ do
        ~(a, w)  <- runWriterT2 m
        ~(b, w') <- runWriterT2 (k a)
        return (b, w `mappend` w')
    {-# INLINE (>>=) #-}
    fail msg = WriterT2 $ fail msg
    {-# INLINE fail #-}

instance (Monoid w, Fail.MonadFail m) => Fail.MonadFail (WriterT2 w m) where
    fail msg = WriterT2 $ Fail.fail msg
    {-# INLINE fail #-}

instance (Monoid w, MonadPlus m) => MonadPlus (WriterT2 w m) where
    mzero       = WriterT2 mzero
    {-# INLINE mzero #-}
    m `mplus` n = WriterT2 $ runWriterT2 m `mplus` runWriterT2 n
    {-# INLINE mplus #-}

instance (Monoid w, MonadFix m) => MonadFix (WriterT2 w m) where
    mfix m = WriterT2 $ mfix $ \ ~(a, _) -> runWriterT2 (m a)
    {-# INLINE mfix #-}

instance (Monoid w) => MonadTrans (WriterT2 w) where
    lift m = WriterT2 $ do
        a <- m
        return (a, mempty)
    {-# INLINE lift #-}

instance (Monoid w, MonadIO m) => MonadIO (WriterT2 w m) where
    liftIO = lift . liftIO
    {-# INLINE liftIO #-}

instance (Monoid w, MonadZip m) => MonadZip (WriterT2 w m) where
    mzipWith f (WriterT2 x) (WriterT2 y) = WriterT2 $
        mzipWith (\ ~(a, w) ~(b, w') -> (f a b, w `mappend` w')) x y
    {-# INLINE mzipWith #-}

-- | @'tell' w@ is an action that produces the output @w@.
tell :: (Monad m) => w -> WriterT2 w m ()
tell w = writer ((), w)
{-# INLINE tell #-}

-- | @'listen' m@ is an action that executes the action @m@ and adds its
-- output to the value of the computation.
--
-- * @'runWriterT2' ('listen' m) = 'liftM' (\\ (a, w) -> ((a, w), w)) ('runWriterT2' m)@
listen :: (Monad m) => WriterT2 w m a -> WriterT2 w m (a, w)
listen m = WriterT2 $ do
    ~(a, w) <- runWriterT2 m
    return ((a, w), w)
{-# INLINE listen #-}

-- | @'listens' f m@ is an action that executes the action @m@ and adds
-- the result of applying @f@ to the output to the value of the computation.
--
-- * @'listens' f m = 'liftM' (id *** f) ('listen' m)@
--
-- * @'runWriterT2' ('listens' f m) = 'liftM' (\\ (a, w) -> ((a, f w), w)) ('runWriterT2' m)@
listens :: (Monad m) => (w -> b) -> WriterT2 w m a -> WriterT2 w m (a, b)
listens f m = WriterT2 $ do
    ~(a, w) <- runWriterT2 m
    return ((a, f w), w)
{-# INLINE listens #-}

-- | @'pass' m@ is an action that executes the action @m@, which returns
-- a value and a function, and returns the value, applying the function
-- to the output.
--
-- * @'runWriterT2' ('pass' m) = 'liftM' (\\ ((a, f), w) -> (a, f w)) ('runWriterT2' m)@
pass :: (Monad m) => WriterT2 w m (a, w -> w) -> WriterT2 w m a
pass m = WriterT2 $ do
    ~((a, f), w) <- runWriterT2 m
    return (a, f w)
{-# INLINE pass #-}

-- | @'censor' f m@ is an action that executes the action @m@ and
-- applies the function @f@ to its output, leaving the return value
-- unchanged.
--
-- * @'censor' f m = 'pass' ('liftM' (\\ x -> (x,f)) m)@
--
-- * @'runWriterT2' ('censor' f m) = 'liftM' (\\ (a, w) -> (a, f w)) ('runWriterT2' m)@
censor :: (Monad m) => (w -> w) -> WriterT2 w m a -> WriterT2 w m a
censor f m = WriterT2 $ do
    ~(a, w) <- runWriterT2 m
    return (a, f w)
{-# INLINE censor #-}

-- | Lift a @callCC@ operation to the new monad.
liftCallCC :: (Monoid w) => CallCC m (a,w) (b,w) -> CallCC (WriterT2 w m) a b
liftCallCC callCC f = WriterT2 $
    callCC $ \ c ->
    runWriterT2 (f (\ a -> WriterT2 $ c (a, mempty)))
{-# INLINE liftCallCC #-}

-- | Lift a @catchE@ operation to the new monad.
liftCatch :: Catch e m (a,w) -> Catch e (WriterT2 w m) a
liftCatch catchE m h =
    WriterT2 $ runWriterT2 m `catchE` \ e -> runWriterT2 (h e)
{-# INLINE liftCatch #-}

