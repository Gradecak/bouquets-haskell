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
  show (StemAmount a b) = show a ++ show b

data Design = Design
  { name :: Char,
    size :: Size,
    stemAmounts :: [StemAmount],
    capacity :: Int
  }

instance Show Design where
  show (Design n s stems cap) = show n ++ show s ++ show stems ++ show cap

designStems :: Design -> [Stem]
designStems (Design _ _ [] _) = []
designStems (Design n size (x : xs) c) = Stem (species x) size : designStems (Design n size xs c)

designArrangements :: Design -> [[StemAmount]]
designArrangements (Design _ _ stems cap) = stemArrangements stems cap

options :: StemAmount -> [StemAmount]
options (StemAmount 0 species) = []
options s = s : options s{maxAmount=maxAmount s - 1 }

stemArrangements :: [StemAmount] -> Int -> [[StemAmount]]
stemArrangements [] 0 = [[]]
stemArrangements [] p = []
stemArrangements (x : xs) amount =
  [o : y | o <- options x, y <- stemArrangements xs (amount - maxAmount o)]

type Bouquet = Design
