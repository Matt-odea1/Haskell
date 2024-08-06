module MoveGenerator where

-- Feel free to import more stuff here, as long as it's all from `base`. In other words, importing it should not require tinkering with the stack.yaml or .cabal files.
import Data.Maybe(fromMaybe)
import Data.List(intersperse,sort,nub,delete)
import Data.Char(toLower)
import Control.Monad((>=>))
import Test.QuickCheck

{- A dictionary, for our purposes, is a set of words.
   A word, for our purposes is a String.

   A Trie is a data structure for representing dictionaries.  Its main advantage is that many operations, such as lookup and insert, have
   runtime that's proportional to the length of the word, not the size of the dictionary.

   A node `Trie b ts` consists of:
   - `b::Bool` indicating whether the empty word is included in the
     dictionary or not.
   - A list `ts::[(Char,Trie)]` of pairs `(x,t)` of characters `x` and associatied subtries `t`. The intention is that
     x:xs is in the dictionary `Trie b ts` whenever `xs` is in the dictionary `t`.
     In a well-formed Trie, these lists must be sorted by character, and cannot contain duplicate entries for any character.

   For example, the dictionary ["he","hell","hello","hi"] would be represented by the following Trie:

                False
                  | 'h'
                False
          'e'  /     \ 'i'
              /       \
           True       True
             | 'l'
           False
             | 'l'
           True
             | 'o'
           True
            
   ...which looks like this in our Haskell representation:

Trie False
  [ ('h',
      Trie False
        [ ('e',
            Trie True
              [ ('l',
                  Trie False
                    [ ('l',
                        Trie True
                          [ ('o',
                              Trie True [])
                          ]
                      )
                    ]
                )
              ]
          )
        ]
    )
  , ('i',
      Trie True
        []
    )
  ]
 -}
data Trie = Trie Bool [(Char,Trie)] 
  deriving (Eq,Show)


{- `empty` represents an empty dictionary. -}
empty :: Trie
empty = Trie False []

{- `single xs` represents a dictionary consisting of only `xs`. -}
single :: String -> Trie
single []     = Trie True []
single (x:xs) = Trie False [(x,single xs)]

{- `insert t xs` inserts the word xs into the dictionary t. -}
insert :: String -> Trie -> Trie -- takes a string and trie, inserts into the trie
insert [] (Trie _ ts)     = Trie True ts -- inserting nothing returns the same trie, with nothing as a valid word
insert (x:xs) (Trie b ts) =
  case span ((<x) . fst) ts of -- splits ts into 2 lists, 0-->last element less than x (ts1), x-->end (ts2)
    (ts1,[]) -> Trie b $ ts1 ++ [(x,single xs)] -- if there are no characters less than x (ie. ts1 is the whole list), we should insert the new word
    (ts1,(y,t):ts2) -- if there are chars before x, 
      | x == y    -> Trie b $ ts1 ++ (x,insert xs t):ts2 -- first, check if the char matches our target. If so, look to go deeper until we can insert the remainder
      | otherwise -> Trie b $ ts1 ++ (x,single xs):(y,t):ts2 -- if the char doesn't match, the char is new in this bit of the trie, and we should insert the char and then look to insert the rest

{- `toList t` gives the list of all words in t. If `t` is well-formed, they will come out in alphabetical order.
 -}
toList :: Trie -> [String]
toList (Trie b ts) =
  first ++ rest where
  first | b         = [""]
        | otherwise = []
  rest = concatMap (\(x,t) -> map (x:) $ toList t) ts

{- `fromList ws` should return a dictionary containing exactly the words in ws.
 -}
fromList :: [String] -> Trie
fromList ws = foldr insert empty ws -- insert each word recursively

{- Recall that a Trie is well-formed if all the [(Char,Trie)] lists in it have the following properties:
   - they are sorted in ascending order by the Char
   - they contain at most one entry for each Char.

  Write a predicate `wellFormed` that returns True if the given Trie is well-formed, and False otherwise.

  Note that you are responsible for maintaining well-formedness: all functions you write that produce
  tries should return a well-formed trie, if their argument tries (if any) are all well-formed.
 -}
wellFormed :: Trie -> Bool
wellFormed (Trie b ts) =
    sortedCheck (map fst ts)
    && noDupesCheck (map fst ts)
    && all (\(_, t) -> wellFormed t) ts
  where
    sortedCheck xs = xs == sort xs
    noDupesCheck xs = length xs == length (nub xs)

{- We say that a trie is *minimal* if it contains no dead branches. 
   A dead branch is a subtrie containing no words. (As a special case, `empty` counts as minimal).

   Here is a minimal trie and a non-minimal trie, both representing the same dictionary.

       False               False
         | 'h'       'h' /     \ 'k'
       True            True    False
                  'a'  /   \ 'i'
                    False False

       ^^Minimal      ^^Not Minimal

   The non-minimal tree above has three dead branches.
   The only way to make it minimal is to cut off all three, obtaining the tree on the left.

   Write a predicate `minimal` that returns True if the given Trie is minimal, and False otherwise.

   Note that well-formedness and minimality are orthogonal:
   it's possible for a Trie to be minimal and not well-formed, or vice versa.
 -}
minimal :: Trie -> Bool
minimal empty = True
minimal (Trie b ts) =
    (b || not (null ts)) && all (\(_, t) -> minimal t) ts

prune :: Trie -> Trie
prune (Trie b ts) = Trie b prunedTs
  where
    prunedTs = filter (not . isDead . snd) $ map (\(c, t) -> (c, prune t)) ts
    isDead (Trie b ts) = not b && null ts

{- Here's a generator and associated
   Arbitrary instance for use with QuickCheck.
   This should only generate well-formed Tries,
   but is not guaranteed to generate minimal Tries.

   It's *not* necessary to fully understand what's going on here.
 -}
genTrie :: Int -> Gen Trie
genTrie 0 = pure $ Trie True []
genTrie n =
  Trie <$> arbitrary <*> (genKeys >>= genSubtries) where
  genKeys :: Gen [Char]
  genKeys = sort . nub <$> (resize 5 . listOf $ elements ['a'..'z'])
  genSubtries :: [Char] -> Gen [(Char,Trie)]
  genSubtries cs =
      zip cs <$> vectorOf (length cs) (genTrie . max 0 $ n-1-length cs)

instance Arbitrary Trie where
  arbitrary = sized $ genTrie . min 15
  shrink (Trie b ts) =
    (Trie b <$> shrinkList (const []) ts) ++
    (Trie b <$> map shrink ts)

{- `check t xs` should return True if `xs` is in the dictionary `t`, and False otherwise.
 -}
check :: Trie -> String -> Bool
check (Trie b _) [] = b
check (Trie _ ts) (c:cs) = 
  case lookup c ts of
    Nothing -> False
    Just t  -> check t cs

{- The union of two dictionaries t,t' should contain all words that occur in either t or t'.
 -}
union :: Trie -> Trie -> Trie
union (Trie b1 ts1) (Trie b2 ts2) =
  Trie (b1 || b2) (combine ts1 ts2)
  where
    combine :: [(Char, Trie)] -> [(Char, Trie)] -> [(Char, Trie)]
    combine [] ys = ys
    combine xs [] = xs
    combine xs ((y, yt) : ys) =
      case lookup y xs of
        Nothing -> (y, yt) : combine xs ys
        Just xt -> (y, union xt yt) : combine (delete (y, xt) xs) ys
      -- if is true for one, then is true for both
      -- if element exists, fold elements of children left into right children, map if not in right, then add in children
      -- if element does not exist, add in all elements from left into right, including all children

{- The intersection of two dictionaries t,t' should contain all words that occur in *both* t and t'.
 -}
intersection :: Trie -> Trie -> Trie
intersection t1 t2 = difference (union t1 t2) (union (difference t1 t2) (difference t2 t1))
  where
    difference :: Trie -> Trie -> Trie
    difference (Trie b ts) (Trie b' ts') =
      Trie (b && not b') (concatMap entryDiff ts)
      where
        entryDiff :: (Char, Trie) -> [(Char, Trie)]
        entryDiff (x, t) =
          case lookup x ts' of
            Nothing -> [(x, t)]
            Just t' -> dropEmpty (x, difference t t')
    
    dropEmpty :: (Char, Trie) -> [(Char, Trie)]
    dropEmpty (_, Trie False []) = []
    dropEmpty t = [t]
{- One of the above (union or intersection) forms a monoid with `empty` as identity element.
   Use the right one to define the following monoid instance:
 -}
newtype TrieMonoid = TrieMonoid {fromMonoid :: Trie} deriving (Eq,Show)

instance Semigroup TrieMonoid where
  TrieMonoid t1 <> TrieMonoid t2 = TrieMonoid (union t1 t2)

instance Monoid TrieMonoid where
  mappend = (<>)
  mempty =  TrieMonoid empty

-- ========================================================================================================================================
{- In the remainder of the assignment, we will use our Trie library above to develop a move generator for Scrabble-like word games.

   A player makes a *move* by placing a sequence of tiles on the board, either horizontally or vertically. These tiles will
   connect with the pre-existing letters on the board to form words. A move is *legal* if every word thus formed occurs in the dictionary.

   A move generator is a core component for any word game program.
   It takes the following inputs:
   - A Trie, representing the dictionary
   - An Int, representing the number of tiles the player will place. To find all moves, we would need to run the move generator once for
     every possible number of tiles, and combine the results.
   - A Rack. This represents the player's pool of letter tiles available for play.
   - A Board. This represents the current state of the board.
   It produces the following output:
   - A Trie, representing the subset of the dictionary that are legal legal moves of the desired length.

   We make a number of simplifying assumptions:
   - We will only generate moves that start from a particular fixed square on the board.
   - We only consider horizontal moves.
 -}

{- pick x xs is a utility function which should satisfy the following properties:

   - If `elem x xs`, then `pick x xs = (True,ys)` for some `ys`
                     such that `xs` is a permutation of `x:ys`.
   - If `not(elem x xs)`, then `pick x xs = (False,ys)`, for some `ys`
                          such that `xs` is a permutation of `ys`.

   This utility function is useful for pulling out specific tiles from a rack.
============================================================================================================================================
 -}

pick :: Eq a => [a] -> a -> (Bool, [a])
pick [] _ = (False, [])
pick (x:xs) y
  | x == y    = (True, xs)
  | otherwise = (found, x : rest)
  where (found, rest) = pick xs y

{- `sandwichableLetters t xs ys` should return a list containing all the characters x such that `xs++x:ys` is a word in `t`.

   This will be handy for collecting constraints imposed by words formed vertically.
 -}
sandwichableLetters :: Trie -> String -> String -> [Char]
sandwichableLetters t xs ys = [x | x <- ['a'..'z'], check t (xs ++ [x] ++ ys)]

{- A constraint represents a predicate on characters.
   A character c is said to *match* a constraint according to the following clauses:
  
   - Any character c matches `Wildcard`.
   - A character c matches `Mem cs`, if c occurs in cs.
 -}
data Constraint = Wildcard | Mem String deriving (Show,Eq)

{- QuickCheck generator for constraints.
   Not necessary to follow all the details. -}
instance Arbitrary Constraint where
  arbitrary = oneof [pure Wildcard,
                     Mem . sort . nub <$> listOf(elements ['a'..'z'])]
  shrink Wildcard = []
  shrink (Mem xs) = Mem <$> shrinkList (:[]) xs

{- A Pattern is a list of Constraints.
   We say that a word xs matches a pattern cs if:
   - `length xs == length cs`
   - The i:th character of xs matches the i:th constraint in cs
     for all i.

   This is a simplified form of regular expressions, whose full
   generality we will not need here.
 -}
type Pattern = [Constraint]

-- Main function to filter the Trie for words of a specific length
filterLength :: Int -> Trie -> Trie
filterLength n (Trie b ts) = Trie b (filterLengthHelper n 1 ts)
  where
    filterLengthHelper :: Int -> Int -> [(Char, Trie)] -> [(Char, Trie)]
    filterLengthHelper _ _ [] = []
    filterLengthHelper targetDepth currentDepth ts
      | currentDepth == targetDepth = [(c, Trie subB []) | (c, Trie subB _) <- ts, subB]
      | otherwise = [(c, Trie subB (filterLengthHelper targetDepth (currentDepth + 1) subTs)) 
                    | (c, Trie subB subTs) <- ts, not (null (filterLengthHelper targetDepth (currentDepth + 1) subTs))]

    -- Helper function to check if a Trie node has children
    hasChildren :: Trie -> Bool
    hasChildren (Trie _ children) = not (null children)

{-
OPTION USING LISTS
filterLength :: Int -> Trie -> Trie
filterLength n t = fromList (filter (\w -> length w == n) (toList t))
-}


{- `filterPattern cs t` should return
   a dictionary containing all words in t that matches the pattern cs.

A Pattern is a list of Constraints.
   We say that a word xs matches a pattern cs if:
   - `length xs == length cs`
   - The i:th character of xs matches the i:th constraint in cs
     for all i.
 -}
filterPattern :: Pattern -> Trie -> Trie
filterPattern [] (Trie b ts) = Trie b []  -- Base case: empty pattern matches all words in the current Trie
filterPattern (Wildcard:cs) (Trie b ts) = Trie b smallerSubTree
  where
    smallerSubTree = [(c, filterPattern cs subtrie) | (c, subtrie) <- ts]  -- Recursively filter each subtrie with the remaining pattern

filterPattern ((Mem ms):cs) (Trie b ts) = Trie b filteredSubtries
  where
    filteredSubtries = [(c, filterPattern cs subtrie) | (c, subtrie) <- ts, elem c ms]  -- Filter subtries that match the current Mem constraint


{- A Tile is either a letter tile, or a blank tile.
   Blank tiles are the most OP thing in the game:
   they are wildcards that can be played as any letter.

   A Rack is just a list of Tiles..
 -}
data Tile = Letter Char | Blank deriving (Eq,Show)
type Rack = [Tile]

{- QuickCheck generator for tiles.
   Very biased towards Blank tiles. -}
instance Arbitrary Tile where
  arbitrary = oneof [pure Blank,
                     Letter <$> elements ['a'..'z']]

{- `filterPlayables r t` should return all the words in t that can be formed
    by using (a subset of) the tiles in the rack r.

   Note that each tile can only be used once.

   For example, the tiles [Letter 'w', Blank, Letter 'o'] can form many English words, including:

    we, ow, cow, how, who, of, wow, ...

   But not including, e.g. whom, which would require using the Blank twice.

   Of course, the above words are just an example: they need not be present in the input dictionary.
 -}
filterPlayables :: Rack -> Trie -> Trie
filterPlayables [] (Trie b _) = Trie b []  -- Base case: empty rack, return Trie with the current node's boolean value
filterPlayables _ (Trie b []) = Trie b []  -- Base case: empty Trie means no words can be found
filterPlayables rack (Trie b ts) = Trie b (concatMap filterByTileWithRemaining rack)
  where
    filterByTileWithRemaining :: Tile -> [(Char, Trie)]
    filterByTileWithRemaining (Letter c) = [(c', filterPlayables remainingRack subtrie) | (c', subtrie) <- ts, c == c']
      where remainingRack = removeOne (Letter c) rack
    filterByTileWithRemaining Blank = [(c, filterPlayables (removeOne Blank rack) subtrie) | (c, subtrie) <- ts]

-- Function to remove one occurrence of a tile from the rack
removeOne :: Eq a => a -> [a] -> [a]
removeOne _ [] = []
removeOne y (x:xs)
  | x == y    = xs
  | otherwise = x : removeOne y xs


{- `Board xs` represents a view of the board state as follows:
   - each element of xs represents a column
   - the first element of xs is the column where the player's first
     tile is placed.
   - the second element of xs represents the column immediately to the
     right of the column where the player's first tile is placed,
     and so on.
   - The contents of each column is represented by a tuple `((as,bs),r)`
     as follows:
     - r represents the contents of the row where the player makes their
       move (the *main row*), as follows:
       - if `r == Nothing`, there is no pre-existing tile in this square.
       - if `r == Just c`, there is a pre-existing tile with letter `c`
         in this square. Additional tiles can't be stacked on top,
         but the player can build around it.
     - `as` represents the longest contiguous sequence of letters
       immediately above the main row, from top to bottom.
       If `as = []`, it means the square above the main row is empty.
     - `bs` represents the longest contiguous sequence of letters
       immediately below the main row, from top to bottom.
       If `bs = []`, it means the square above the main row is clear.
   - The board does not extend beyond xs. Words that would go beyond
     the board are illegal moves and cannot be played.
 -}
newtype Board = Board [((String,String),Maybe Char)] deriving (Eq,Show)

{- `moves t n r b` is the big one: the move generator!

   This function should return the set of all legal moves that play n tiles from the rack r onto the board b, where t is the dictionary.

   Almost all the functions you wrote above are designed to be useful when writing the move generator, but it will take some non-trivial
   thinking to figure out how exactly.
 -}
createMainRowPattern :: Board -> Rack
createMainRowPattern (Board []) = []
createMainRowPattern (Board cols) =
  concatMap patternFromColumn cols
  where
    patternFromColumn :: ((String, String), Maybe Char) -> Rack
    patternFromColumn ((as, bs), mc) =
      case mc of
        Just c -> [Letter c]
        Nothing -> []

createVerticalConstraints :: Board -> Trie -> Pattern
createVerticalConstraints (Board []) _ = []
createVerticalConstraints (Board (((as, bs), mc):cols)) t =
  case mc of
    Just c -> Mem [c] : createVerticalConstraints (Board cols) t  -- Use the specified letter if present
    Nothing ->
      if null as && null bs
        then Wildcard : createVerticalConstraints (Board cols) t  -- Use Wildcard if no letters above or below
        else Mem (sandwichableLetters t as bs) : createVerticalConstraints (Board cols) t  -- Use sandwichable letters otherwise

moves :: Trie -> Int -> Rack -> Board -> Trie
moves t n r (Board cols) =
  let
    verticalConstraints = createVerticalConstraints (Board cols) t
    validVerticalWords = filterPattern verticalConstraints t
    validPlayable = filterPlayables (r++(createMainRowPattern (Board cols))) t -- really really big
    validPlayableWords = intersection validVerticalWords validPlayable
  in
    filterLength n (validPlayableWords)

type MoveGenerator = Trie -> Int -> Rack -> Board -> Trie
{- Write a function which, given a move generator `g` such as the one you wrote above, returns all playable moves of *any*
   length (according to `g`).

   You may find it useful to use the monoid instance you wrote above.

   To generate all moves according to *your* move generator, you would invoke `allMoves moves`.  This layer of indirection makes it
   possible for us to test `allMoves` independently of `moves` by plugging in our reference implementation.
 -}
allMoves :: MoveGenerator -> Trie -> Rack -> Board -> Trie
allMoves g dict rack board =
  let
    maxMoveLength = 7  -- Example: Up to 7-letter words
    allGeneratedMoves = mconcat [TrieMonoid (g dict len rack board) | len <- [1..maxMoveLength]]
  in
    fromMonoid allGeneratedMoves

{- TODO:
   If you've consistently used Tries instead of lists ---
   in particular, if you've refrained from using the fromList . doStuff . toList
   trick explained in the assignment spec, and if your assignment solution scores 18/20
   or more, then you are eligible for two bonus points on the final exam.

   Set trieOrDie to True to certify that you have used Tries consistently.
 -}
trieOrDie :: Bool
trieOrDie = True
-- ============================================================================================================================================

{- Once you've implemented fromList, you can use this function to read a dictionary from a file of
   whitespace-separated words. Run the following in GHCi:

     myDict <- dictionaryFromFile "dictionary.txt"

   This will save the dictionary comprised of the words in dictionary.txt into the variable binding myDict.
   For now, don't worry too much about how this works!  It will become clearer later in the course.
 -}
dictionaryFromFile :: String -> IO Trie
dictionaryFromFile =
  readFile >=> return . fromList . words . map toLower

{- There's no need to understand the following code! What is does is prints out boards as ASCII art.
   Try running e.g. drawBoard at_board
 -}
drawBoard :: Board -> IO()
drawBoard (Board []) = return ()
drawBoard (Board xs) =
  putStrLn $ concat
    [as',
     ">",
     intersperse '|' $ fromMaybe ' ' <$> cs,
     "\n",
     bs'] where
  as = fst . fst <$> xs
  bs = snd . fst <$> xs
  cs = snd <$> xs
  alen  = maximum $ length <$> as
  blen  = maximum $ length <$> bs
  x = length xs
  as' =
    do
      n <- [0..alen-1]
      let xs = getLetAb n as <$> [0..x-1]
      '|':intersperse '|' xs++"|\n"
  bs' =
    do
      n <- [0..blen-1]
      let xs = getLetBe n bs <$> [0..x-1]
      '|':intersperse '|' xs++"|\n"
  getLetAb n ws x =
    let xs = ws !! x in
      if n < alen - length xs then
        ' '
      else xs !! (n + length xs - alen)
  getLetBe n ws x =
    let xs = ws !! x in
      if n < length xs then
        xs !! n
      else
        ' '

{- Here are the various example boards from the spec. -}

empty_board :: Board
empty_board =
  Board
    [(("",""),Nothing),
     (("",""),Nothing),
     (("",""),Nothing),
     (("",""),Nothing),
     (("",""),Nothing),
     (("",""),Nothing),
     (("",""),Nothing),
     (("",""),Nothing)
    ]

aeintsr_rack :: Rack
aeintsr_rack =
  [Letter 'a',
   Letter 'e',
   Letter 'i',
   Letter 'n',
   Letter 's',
   Letter 't',
   Letter 'r'
  ]
  
wo_rack :: Rack
wo_rack =
  [Letter 'w',
   Letter 'o',
   Blank
  ]

dog_board :: Board
dog_board =
  Board
    [(("",""),Nothing),
     (("",""),Nothing),
     (("",""),Nothing),
     (("",""),Nothing),
     (("",""),Nothing),
     (("d","cent"),Nothing),
     (("o",""),Nothing),
     (("g",""),Nothing)
    ]

at_board :: Board
at_board =
  Board
    [(("",""),Nothing),
     (("",""),Nothing),
     (("",""),Just 'a'),
     (("","alk"),Just 't'),
     (("",""),Nothing),
     (("d","cent"),Nothing),
     (("o",""),Nothing),
     (("g",""),Nothing)
    ]
