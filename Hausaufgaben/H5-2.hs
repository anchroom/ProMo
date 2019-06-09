-- | Listen mit bis zu zwei Werten repräsentieren.

data AtMostTwo1 a = None | One a | Two a a
                    deriving (Eq, Show)

type AtMostTwo2 a = Either () (Either a (a, a))

type AtMostTwo3 a = Maybe (a, Maybe a)

-- | data AtMostTwo1 --> type AtMostTwo2
convert1 :: AtMostTwo1 a -> AtMostTwo2 a
convert1 None = Either ()
{-convert1 One a = Right


convert1 None = Left ()

         | One a       = AtMostTwo2 Either (a)
            | (Two (a a)) = AtMostTwo2 (Either a (a, a))
            | otherwise   = error "TODO"

convert2 :: AtMostTwo2 a -> AtMostTwo3 a
convert2 x = error "TODO"

convert3 :: AtMostTwo3 a -> AtMostTwo1 a
convert3 x = error "TODO"
-}