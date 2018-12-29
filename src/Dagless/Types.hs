{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}

{-|
Module      : Dagless.Types
Description : The types used for the Dagless transformer and its derivatives.
Copyright   : (c) Tom Harding, 2018
License     : MIT
Maintainer  : tom.harding@habito.com
Stability   : experimental

This module isn't super interesting, and you'll get more from the 'Dagless'
module directly. Here, we describe 'DaglessT', 'Dagless' (@DaglessT Identity@),
and their prime variations. Both 'DaglessT'' and 'Dagless'' refer to instances
for which the context of the DAG computations and the context of the DSL are
equivalent.
-}
module Dagless.Types where

import           Control.Monad               (join)
import qualified Control.Monad.Indexed       as Ix
import           Control.Monad.Indexed.State (IxStateT (..))
import           Control.Monad.Indexed.Trans (IxMonadTrans (..))
import           Data.Coerce                 (coerce)
import           Data.Functor.Identity       (Identity (..))
import           Data.HDagF                  (HDagF (..), evaluate)

-- | The Dagless transformer. Describes a computation that, as well as the
-- side-effects described by @m@, updates the nodes within a DAG. See the tests
-- folder for more examples.
--
-- - @m@ is the monad we're transforming.
-- - @f@ is the context in which the DAG nodes are evaluated.
-- - @pre@ is the set of nodes in the DAG /before/ the computation.
-- - @post@ is the set of nodes in the DAG /after/ the computation.
-- - @a@ is the value inside the DaglessT context.

newtype DaglessT f m pre post a
  = DaglessT { runDaglessT :: IxStateT m (HDagF f pre) (HDagF f post) a }
  deriving Functor

instance Monad m => Ix.IxFunctor (DaglessT f m) where
  imap f (DaglessT xs) = DaglessT (Ix.imap f xs)

instance Monad m => Ix.IxPointed (DaglessT f m) where
  ireturn x = DaglessT (Ix.ireturn x)

instance Monad m => Ix.IxApplicative (DaglessT f m) where
  iap (DaglessT fs) (DaglessT xs) = DaglessT (Ix.iap fs xs)

instance Monad m => Ix.IxMonad (DaglessT f m) where
  ibind f (DaglessT xs) = DaglessT (xs Ix.>>>= runDaglessT . f)

instance IxMonadTrans (DaglessT f) where
  ilift = DaglessT . ilift

-- | Often, your DAG computations and your state computations will require the
-- same transformer stack of side-effects. To accommodate this, we have a type
-- synonyf mor stacks in which the DAG node context and the transformer context
-- are the same.

type DaglessT' m pre post a
  = DaglessT m m pre post a

-- | If we're not interested in building our transformer stack any further, the
-- 'Dagless' monad simply sets the underlying monad to 'Identity'.

type Dagless f pre post a
  = DaglessT Identity f pre post a

-- | If our DAG computations are pure, we also don't particularly care about
-- the @f@ context, and so we can set that to `Identity` as well, giving us a
-- type that isn't anywhere near as scary.

type Dagless' pre post a
  = DaglessT' Identity pre post a

-- | Extract the root node of the DAG produced in a 'DaglessT' computation.
-- Note the two contexts: the transformer context @m@, and the DAG computation
-- context @f@.

root :: (Functor f, Functor m) => DaglessT f m '[] (x ': xs) a -> m (f x)
root = fmap (evaluate . snd) . (`runIxStateT` HEmptyF) . runDaglessT

-- | When the two contexts are identical, 'root' returns two layers of context
-- unnecessarily. This function takes a 'DaglessT'', and calls 'join' on the
-- result of 'root'. We edge closer and closer to the Fairbairn threshold.

root' :: Monad m => DaglessT' m '[] (x ': xs) a -> m x
root' = join . root
