# hs-btree
Binary tree structure in haskell (based on list operations).
The operations could be better implemented using recursion.
> For reference only!

# example usage
```
import qualified BinarySearchTree
 as Set (T, empty, insert, fromList, toList, member, delete)

t1 = Set.insert 1 (Set.insert 2 Set.empty)

t2 = Set.fromList [2,3,1]
```

```Set.member 3 t1```

``` console
False
```


``` Set.insert 3 t1 == t2 ```

``` console
True
```

```(t1 <> t2) == t2 ```

``` console
True
```
