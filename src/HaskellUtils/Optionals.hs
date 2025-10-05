module HaskellUtils.Optionals (
  mapNothing, mapNothingT,
  leftToMaybe, rightToMaybe, leftToMaybeT
) where

mapNothing :: e -> Maybe a -> Either e a
mapNothing e Nothing = Left e
mapNothing _ (Just a) = Right a

mapNothingT :: Monad m => m e -> MaybeT m a -> EitherT e m a
mapNothingT e ma = do
  a <- lift $ runMaybeT ma
  case a of
    Nothing -> liftLeft e
    Just a' -> rightT a'

leftToMaybe :: Either e a -> Maybe e
leftToMaybe (Left e) = Just e
leftToMaybe _ = Nothing

rightToMaybe :: Either e a -> Maybe a
rightToMaybe (Right a) = Just a
rightToMaybe _ = Nothing

leftToMaybeT :: EitherT e m a -> MaybeT m e
leftToMaybeT (EitherT ma) = MaybeT $ fmap leftToMaybe ma

rightToMaybeT :: EitherT e m a -> MaybeT m a
rightToMaybeT (EitherT ma) = MaybeT $ fmap rightToMaybe ma
