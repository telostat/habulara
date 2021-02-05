module Laws where


-- | Semigroup associativity law.
prop_semigroupAssoc :: (Eq a, Semigroup a) => a -> a -> a -> Bool
prop_semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)


-- | Monoid left identity law.
prop_monoidLeftIdentity :: (Eq a, Monoid a) => a -> Bool
prop_monoidLeftIdentity a = mempty <> a == a


-- | Monoid right identity law.
prop_monoidRightIdentity :: (Eq a, Monoid a) => a -> Bool
prop_monoidRightIdentity a = a <> mempty == a
