module Ex04 where
import Data.Semigroup
import Data.Monoid
import Control.Monad.State (State, get, put, evalState)
import Test.QuickCheck
import Priority
import Size

-- DEFINITIONS AND HELPER FUNCTIONS --

type NodeInfo = (Size, Priority)

data QueueTree a
  = Null
  | Leaf NodeInfo a
  | Node NodeInfo (QueueTree a) (QueueTree a)
  deriving (Show)

nodeInfo :: QueueTree a -> NodeInfo
nodeInfo Null = mempty
nodeInfo (Leaf i _) = i
nodeInfo (Node i _ _) = i

sizeOf :: QueueTree a -> Size
sizeOf = fst . nodeInfo

maxPrio :: QueueTree a -> Priority
maxPrio = snd . nodeInfo

-- checks whether the tree structure
-- is balanced (i.e. that the left subtree and the right
-- subtree don't ever differ too much in size)
balanced :: QueueTree a -> Bool
balanced (Node i l r) =
  let sl = unSize (sizeOf l) in
  let sr = unSize (sizeOf r) in
  abs (sl - sr) <= 1 && balanced l && balanced r
balanced _ = True



-- EXERCISE STARTS HERE --

-- Task 1a. Write a well-formedness predicate
--          wf for the `QueueTree` data type.

-- Hint: Both `Priority` and `Size` are semigroups/monoids.
-- This means that the type `NodeInfo` is also automatically
-- a monoid.

wf :: QueueTree a -> Bool
wf Null = True
wf (Leaf (s, _) _) = unSize s == 1 -- A leaf should have size 1
wf (Node (s, p) l r) =
  let sl = sizeOf l
      sr = sizeOf r
      pl = maxPrio l
      pr = maxPrio r
      expectedSize = sl <> sr <> size 1 -- Combine sizes using monoid instance
      maxPriority = maximum [unPriority p, unPriority pl, unPriority pr]
      expectedPrio = priority maxPriority -- Priority should be the max of all priorities
  in s == expectedSize && p == expectedPrio && wf l && wf r



-- Task 1b. Write smart constructors `leaf` and `node`
--          for the `QueueTree` data type which maintain
--          the well-formedness invariant. I.e. given
--          well-formed inputs, the smart constructors
--          should give well-formed outputs.
--          You should /not/ tweak the structure of the ~QueueTree~
--          beyond updating the ~NodeInfo~; in particular don't do
--          ~node Null Null = Null~.

leaf :: Priority -> a -> QueueTree a
leaf p x = Leaf (size 1, p) x

node :: QueueTree a -> QueueTree a -> QueueTree a
node l r =
  let sl = sizeOf l
      sr = sizeOf r
      pl = maxPrio l
      pr = maxPrio r
      combinedSize = sl <> sr <> size 1
      combinedPriority = priority (maximum [unPriority pl, unPriority pr])
  in Node (combinedSize, combinedPriority) l r



-- Task 2a. Implement the usual priority queue functions
--          for the type `QueueTree`. These are
--          pop - Remove the element from the queue that has the
--               highest priority, which will always be the top
--               element. Return the modified queue, modifying the queue to place the highest value of L or R as the new head of the queue
--               along with the removed element (if any).

pop :: QueueTree a -> (QueueTree a, Maybe a)
pop Null = (Null, Nothing)  -- If the queue is empty, return (Null, Nothing)
pop (Leaf _ x) = (Null, Just x)  -- If there's only one element, pop it and return it
pop (Node info l r) =
  let (poppedInfo, poppedValue) = rootPop info
  in (Node poppedInfo l r, poppedValue)
  where
    rootPop :: NodeInfo -> (NodeInfo, Maybe a)
    rootPop (s, p) =
      let (newTree, maxValue) = removeMaxPriority l r
      in (nodeInfo newTree, maxValue)

    removeMaxPriority :: QueueTree a -> QueueTree a -> (QueueTree a, Maybe a)
    removeMaxPriority Null Null = (Null, Nothing)
    removeMaxPriority l Null = (l, Nothing)
    removeMaxPriority Null r = (r, Nothing)
    removeMaxPriority (Leaf _ x) Null = (Null, Just x)
    removeMaxPriority (Leaf _ x) r = (r, Just x)
    removeMaxPriority l (Leaf _ x) = (l, Just x)
    removeMaxPriority (Node _ ll lr) (Node _ rl rr) =
      if maxPrio ll >= maxPrio rl
        then let (newL, maxValue) = removeMaxPriority lr rr
             in (node (nodeInfo newL) ll newL, maxValue)
        else let (newR, maxValue) = removeMaxPriority l rr
             in (node (nodeInfo newR) rl newR, maxValue)
insert :: Priority -> a -> QueueTree a -> QueueTree a
insert p x Null = leaf p x  -- If the tree is empty (Null), create a new leaf node with the given priority and element.
insert p x (Leaf info y) = node (leaf p x) (Leaf info y)
insert p x (Node info l r) =
  let sl = sizeOf l  -- Size of the left subtree
      sr = sizeOf r  -- Size of the right subtree
  in if unSize sl <= unSize sr
       then node (insert p x l) r
         -- If the size of the left subtree is less than or equal to the size of the right subtree,
         -- insert the new element into the left subtree recursively.
         -- Construct a new node with:
         --   - The updated left subtree after insertion (insert p x l),
         --   - The unchanged right subtree (r).
       else node l (insert p x r)
         -- Otherwise, insert the new element into the right subtree recursively.
         -- Construct a new node with:
         --   - The unchanged left subtree (l),
         --   - The updated right subtree after insertion (insert p x r).


-- Task 2b. Implement a function `fromList` that converts a
--          list of `(Priority, x)` pairs into a well-formed
--          and balanced `QueueTree x` structure.

fromList :: [(Priority, a)] -> QueueTree a
fromList = foldr (uncurry insert) Null

-- Hint: you can use `fromList` to implement an `Arbitrary`
-- instance for `QueueTree`, allowing you to test your work.


-- Task 3. Implement stateful versions of the pop and insert
--         operations above using the `State` type in Haskell's
--         standard mtl library.
--         Implement a `peek` operation which just returns the
--         highest-priority element without changing the
--         state of the queue.
--         Do not use the `state` function in your final
--         implementations!

pop' :: State (QueueTree a) (Maybe a)
pop' = do
  tree <- get
  let (newTree, popped) = pop tree
  put newTree
  return popped

insert' :: Priority -> a -> State (QueueTree a) ()
insert' p x = do
  tree <- get
  let newTree = insert p x tree
  put newTree

peek' :: State (QueueTree a) (Maybe a)
peek' = do
  tree <- get
  return $ case tree of
    Null -> Nothing
    Leaf _ x -> Just x
    Node _ _ _ -> Just $ topElement tree
  where
    topElement :: QueueTree a -> a
    topElement (Leaf _ x) = x
    topElement (Node _ l _) = topElement l
    topElement Null = error "Empty queue has no elements"

-------------------------------- END OF EXERCISE ---------------------------------

-- You can use the following three examples to test your
-- implementations of pop' and insert', and to practice
-- reading `State`-ful functions.

-- Returns the highest priority currently in the `QueueTree`
-- without changing the state.
getMaxPrio' :: State (QueueTree a) Priority
getMaxPrio' =
  get >>= \q ->
  return (maxPrio q)

-- Removes the element with the second-highest priority
-- in the queue.
dip' :: State (QueueTree a) ()
dip' =
  getMaxPrio' >>= \p ->
  pop'        >>= \h1 ->
  pop'        >>= \h2 ->
  case h1 of
    Nothing -> return ()
    Just h1 -> insert' p h1

-- a `State`-free version of dip
dip :: QueueTree Char -> QueueTree Char
dip = evalState $
  dip' >>= \() ->
  get


---------------------------- TESTS -------------------------------