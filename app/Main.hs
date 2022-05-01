module Main where

import Control.Applicative
import Control.Monad (foldM, unless, void)
import Control.Monad.State
  ( StateT,
    get,
    liftIO,
    modify,
    put,
    runStateT,
  )
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as BS (getLine, null)
import Data.ByteString.Char8 (ByteString)
import qualified Data.Map as M (Map, empty, lookup, update)
import Data.Map.Strict (insertWith)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Parser (parseDesign, parseStem)
import System.Exit (exitFailure)
import Types

-- TODO Migrate to Data.HashTable for O(1) lookup/updates since we are already in the IO
-- monad
type Inventory = M.Map Stem Int

data AppState = AppState
  { designs :: [Design],
    inventory :: Inventory
  }

type App = StateT AppState IO ()

-- HOF to continuously read from stdin and apply f to input until newline is
-- detected
untilNewline :: (ByteString -> App) -> App
untilNewline f = liftIO BS.getLine >>= \l -> unless (BS.null l) $ f l >> untilNewline f

readDesign :: ByteString -> App
readDesign line =
  case parseOnly parseDesign line of
    Left err -> liftIO $ print err >> exitFailure
    Right design -> get >>= \(AppState des inv) -> put $ AppState (design : des) inv

processStem :: ByteString -> App
processStem line =
  case parseOnly parseStem line of
    Left err -> liftIO $ print err >> exitFailure
    Right stem -> do
      modify $ \s -> s {inventory = insertWith (+) stem 1 (inventory s)}
      AppState designs inv <- get
      case filter (hasMinimumStock inv) designs of
        (x : _) -> arrangeBouquet inv x
        _ -> return ()

arrangeBouquet :: Inventory -> Design -> App
arrangeBouquet inventory design =
  case _arrangeBouquet inventory design (designArrangements design) of
    Just bouquet -> do
      liftIO (print bouquet)
      modify (\s -> s {inventory = deductBouquet inventory bouquet})
    Nothing -> return ()

-- given the current inventory, design and permutations of the design, try to find the
-- permutation that can be satisfied from the stems in the inventory
_arrangeBouquet :: Inventory -> Design -> [[StemAmount]] -> Maybe Bouquet
_arrangeBouquet inventory _ [] = Nothing
_arrangeBouquet inventory d@(Design name size _ cap) (x : xs) =
  if sum (zipWith (-) inInventory required) >= 0
    then Just $ Design name size x cap
    else _arrangeBouquet inventory d xs
  where
    inInventory = [fromMaybe 0 $ (`M.lookup` inventory) (Stem (species sa) size) | sa <- x]
    required = map maxAmount x

-- given a completed bouquet, return a new map with the bouquet stems deducted from
-- inventory
deductBouquet :: Inventory -> Bouquet -> Inventory
deductBouquet inventory (Design _ size stemAmounts _) =
  foldl (\inv (stem, amount) -> M.update (fn amount) stem inv) inventory toDeduct
  where
    toDeduct = [(Stem (species s) size, maxAmount s) | s <- stemAmounts]
    fn used inStorage = if inStorage - used <= 0 then Nothing else Just $ inStorage - used

-- check if for a given design we have at least 1 of each stem, and the total in storage
-- is >= capacity of the bouquet
hasMinimumStock :: Inventory -> Design -> Bool
hasMinimumStock inventory (Design _ size stemAmounts cap) =
  case sum <$> sequence stockAmounts of
    Nothing -> False
    Just a | a >= cap -> True
    _ -> False
  where
    stockAmounts = [(`M.lookup` inventory) (Stem (species sa) size) | sa <- stemAmounts]

runApp :: App
runApp = untilNewline readDesign >> untilNewline processStem

main :: IO ()
main = void $ runStateT runApp (AppState [] M.empty)
