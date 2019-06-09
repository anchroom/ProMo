-- | a)  Definieren Sie eine Show-Instanz f¨ur den Typ List a unter der Annahme Show a.
-- Die Listenelemente sollen in eckigen Klammern durch Semikolon getrennt angezeigt werden.

data List a = Nil | Cons a (List a) | RepeatCons Int a (List a)

instance Show a => Show (List a) where
  show xs = "[" ++ show' xs ++ "]"
    where
      show' Nil = ""
      show' ( Cons x Nil) = show x
      --show' ( Cons [] xs) = show xs
      show' ( Cons x xs) = show x ++ "; " ++ show' xs
      show' ( RepeatCons 1 x xs) = show' ( Cons x xs)
      show' ( RepeatCons n x xs) = if n <= 0 then show xs else show' ( Cons x (RepeatCons (n-1) x xs))


-- | b)   Machen Sie List a zu einer Instanz von Eq unter der Annahme, dass a Instanz von Eq a ist.
--

instance Eq a => Eq (List a) where
  Nil == Nil                                                = True
  (Cons x xs) == (Cons y ys)                                = (x == y) && (xs == ys)
  (RepeatCons 1 x xs) == (Cons y ys)                        = (x == y) && (xs == ys)
  (RepeatCons n x xs) == (Cons y ys)                        = (x == y) && ((RepeatCons (n-1) x xs) == ys)
  _ == _                     = False




