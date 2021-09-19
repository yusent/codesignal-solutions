data ListNode a = ListNode { val :: a, next :: ListNode a } | Nil deriving Show

removeKFromList :: ListNode a -> a -> ListNode a
removeKFromList Nil _ = Nil
removeKFromList (ListNode v n) k
  | v == k = removeKFromList n k
  | otherwise = ListNode v $ removeKFromList n k
