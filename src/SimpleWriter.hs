module SimpleWriter
  ( Writer
  , runWriter
  , writer
  ) where

-- | Container for annotated values.
newtype Writer a = Writer (a, [String]) deriving (Eq, Show)

instance Functor Writer where

  -- Map a function over the annotated value, keeping
  -- the annotation unchanged.
  fmap f (Writer (x, w)) = Writer (f x, w)

instance Applicative Writer where

  -- Create a Writer without annotations.
  pure x = Writer (x, [])

  -- Apply an annotated function to an annotated value.
  Writer (f, w1) <*> Writer (x, w2) = Writer (f x, w1 ++ w2)

instance Monad Writer where

  -- Feed an annotated value to a function that expects
  -- a normal value and returns annotated one.
  Writer (x, w) >>= f = Writer (x', w ++ w')
    where
      Writer (x', w') = f x

-- | Extract an annotated value from a Writer.
runWriter (Writer (x, xss)) = (x, xss)

-- | Create a Writer from an annotated value.
writer :: (a, [String]) -> Writer a
writer (x, xss) = Writer (x, xss)
