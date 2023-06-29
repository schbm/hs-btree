module BinarySearchTree.ExampleUsage where

import qualified BinarySearchTree
 as Set (T, empty, insert, fromList, toList, member, delete)

t1 = Set.insert 1 (Set.insert 2 Set.empty)

t2 = Set.fromList [2,3,1]

-- >>> Set.member 3 t1
-- False

-- >>> t1 == t2


-- >>> Set.insert 3 t1 == t2
-- True

-- >>> (t1 <> t2) == t2
-- True