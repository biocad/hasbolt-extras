{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE StandaloneDeriving #-}

module Database.Bolt.Extras.Selector.Path where

import           Database.Bolt.Extras.Selector.Types (NodeSelector, RelSelector)

data PathEnd = NodeEnd | RelEnd

data PathSelector (e :: PathEnd) where
  PStartNode  :: NodeSelector                          -> PathSelector 'NodeEnd
  PEndNode    :: PathSelector 'RelEnd  -> NodeSelector -> PathSelector 'NodeEnd
  PDirected   :: PathSelector 'NodeEnd -> RelSelector  -> PathSelector 'RelEnd
  PUndirected :: PathSelector 'NodeEnd -> RelSelector  -> PathSelector 'RelEnd

deriving instance Show (PathSelector e)
deriving instance Eq (PathSelector e)

infixl 8 -:
(-:) :: PathSelector 'RelEnd -> NodeSelector -> PathSelector 'NodeEnd
ps -: ns = PEndNode ps ns

infixl 8 !:-
(!:-) :: NodeSelector -> RelSelector -> PathSelector 'RelEnd
ns !:- rs = PUndirected (PStartNode ns) rs

infixl 8 !:>
(!:>) :: NodeSelector -> RelSelector -> PathSelector 'RelEnd
ns !:> rs = PDirected (PStartNode ns) rs

infixl 8 -:-
(-:-) :: PathSelector 'NodeEnd -> RelSelector -> PathSelector 'RelEnd
ps -:- rs = PUndirected ps rs

infixl 8 -:>
(-:>) :: PathSelector 'NodeEnd -> RelSelector -> PathSelector 'RelEnd
ps -:> rs = PDirected ps rs
