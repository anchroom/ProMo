-- | Listen mit bis zu zwei Werten repräsentieren.

data AtMostTwo1 a = None | One a | Two a a
                    deriving (Eq, Show)

type AtMostTwo2 a = Either () (Either a (a, a))

type AtMostTwo3 a = Maybe (a, Maybe a)

-- | data AtMostTwo1 --> type AtMostTwo2
convert1 :: AtMostTwo1 a -> AtMostTwo2 a
convert1 None = Left ()
convert1 ( One x ) = Right ( Left x )
convert1 ( Two x y ) = Right ( Right (x , y ) )

convert2 :: AtMostTwo2 a -> AtMostTwo3 a
convert2 ( Left () ) = Nothing
convert2 ( Right ( Left x ) ) = Just (x , Nothing )
convert2 ( Right ( Right (x , y ) ) ) = Just (x , Just y )

convert3 :: AtMostTwo3 a -> AtMostTwo1 a
convert3 Nothing = None
convert3 ( Just (x , Nothing ) ) = One x
convert3 ( Just (x , Just y ) ) = Two x y