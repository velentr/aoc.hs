module Queue
  ( empty
  , fromList
  , peek
  , pop
  , pop'
  , push
  , Queue
  ) where

data Queue a = Queue { h :: [a]
                     , t :: [a]
                     } deriving (Show)

empty :: Queue a
empty = Queue { h = [], t = [] }

fromList :: [a] -> Queue a
fromList lst = Queue { h = lst, t = [] }

peek :: Queue a -> Maybe a
peek q = let (elt, _) = pop' q in elt

pop :: Queue a -> Queue a
pop q = let (_, q') = pop' q in q'

pop' :: Queue a -> (Maybe a, Queue a)
pop' q@Queue { h = [], t = [] } = (Nothing, q)
pop' Queue { h = hd, t = [] } =
  let elt:tl = reverse hd in
    (Just elt, Queue { h = [], t = tl })
pop' Queue { h = hd, t = elt:tl } =
  (Just elt, Queue { h = hd, t = tl })

push :: Queue a -> a -> Queue a
push Queue { h = hd, t = tl } elt =
  Queue { h = elt:hd, t = tl }
