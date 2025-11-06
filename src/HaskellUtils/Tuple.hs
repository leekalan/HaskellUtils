module HaskellUtils.Tuple where

setFst :: a -> (a, b) -> (a, b)
setFst a (_, c) = (a, c)

setSnd :: b -> (a, b) -> (a, b)
setSnd b (a, _) = (a, b)

