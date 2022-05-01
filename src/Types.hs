module Types where

data Size
  = L
  | S
  deriving (Show, Eq, Ord)

data Stem = Stem Char Size deriving (Show, Eq, Ord)

data StemAmount = StemAmount
  { maxAmount :: Int,
    species :: Char
  }

instance Show StemAmount where
  show (StemAmount a b) = show a ++ [b]

data Design = Design
  { name :: Char,
    size :: Size,
    stemAmounts :: [StemAmount],
    capacity :: Int
  }

instance Show Design where
  show (Design n s stems cap) = [n] ++ show s ++ foldr (\c a -> show c ++ a) "" stems

designStems :: Design -> [Stem]
designStems (Design _ _ [] _) = []
designStems (Design n size (x : xs) c) = Stem (species x) size : designStems (Design n size xs c)

type Bouquet = Design
