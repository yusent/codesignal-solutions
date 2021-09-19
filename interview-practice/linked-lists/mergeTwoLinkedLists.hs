data ListNode a = ListNode { val :: a, next :: ListNode a } | Nil deriving Show

mergeTwoLinkedLists :: ListNode a -> ListNode a -> ListNode a
mergeTwoLinkedLists Nil l = l
mergeTwoLinkedLists l Nil = l
mergeTwoLinkedLists l1 l2
  | val l1 <= val l2 = ListNode (val l1) $ mergeTwoLinkedLists l2 $ next l1
  | otherwise = ListNode (val l2) $ mergeTwoLinkedLists l1 $ next l
