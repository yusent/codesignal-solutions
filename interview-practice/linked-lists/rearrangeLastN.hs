data ListNode a = ListNode { val :: a, next :: ListNode a } | Nil deriving Show

rearrangeLastN :: ListNode a -> Int -> ListNode a
rearrangeLastN l n = let (h1, h2) = split l (len l - n) in conc h2 h1

split :: ListNode a -> Int -> (ListNode a, ListNode a)
split l 0 = (Nil, l)
split l i = let (h1, h2) = split (next l) (i - 1) in (ListNode (val l) h1, h2)

conc :: ListNode a -> ListNode a -> ListNode a
conc Nil l = l
conc l Nil = l
conc l1 l2 = ListNode (val l1) $ conc (next l1) l2

len :: ListNode a -> Int
len Nil = 0
len (ListNode _ nxt) = 1 + len nxt
