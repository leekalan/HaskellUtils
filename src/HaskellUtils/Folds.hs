module HaskellUtils.Folds where

foldsrUntil :: (a -> b -> Either b c) -> a -> Either b c -> Either b c
foldsrUntil f a (Left b) = f a b
foldsrUntil _ _ (Right c) = Right c

foldsrUntilThen :: (a -> b -> Either b c) -> (a -> c -> c) -> a -> Either b c -> Either b c
foldsrUntilThen f _ a (Left b) = f a b
foldsrUntilThen _ g a (Right c) = Right $ g a c

foldsrUntilA :: Applicative f => (a -> b -> f (Either b c)) -> a -> Either b c -> f (Either b c)
foldsrUntilA f a (Left b) = f a b
foldsrUntilA _ _ (Right c) = pure $ Right c

foldsrUntilThenA :: Applicative f => (a -> b -> f (Either b c)) -> (a -> c -> f c) -> a -> Either b c -> f (Either b c)
foldsrUntilThenA f _ a (Left b) = f a b
foldsrUntilThenA _ g a (Right c) = Right <$> g a c


foldslUntil :: (b -> a -> Either b c) -> Either b c -> a -> Either b c
foldslUntil = flip . foldsrUntil . flip

foldslUntilThen :: (b -> a -> Either b c) -> (c -> a -> c) -> Either b c -> a -> Either b c
foldslUntilThen f = flip . foldsrUntilThen (flip f) . flip

foldslUntilA :: Applicative f => (b -> a -> f (Either b c)) -> Either b c -> a -> f (Either b c)
foldslUntilA = flip . foldsrUntilA . flip

foldslUntilThenA :: Applicative f => (b -> a -> f (Either b c)) -> (c -> a -> f c) -> Either b c -> a -> f (Either b c)
foldslUntilThenA f = flip . foldsrUntilThenA (flip f) . flip