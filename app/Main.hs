module Main where

import           Control.Applicative
import           Control.Monad                    (foldM, unless, void)
import           Control.Monad.State              (StateT, get, liftIO, modify,
                                                   put, runStateT)
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString                  as BS (getLine, null)
import           Data.ByteString.Char8            (ByteString)
import qualified Data.Map                         as M (Map, empty, fromList,
                                                        lookup, update)
import           Data.Map.Strict                  (insertWith)
import           Data.Maybe                       (catMaybes, fromMaybe,
                                                   mapMaybe)
import           Parser                           (parseDesign, parseStem)
import           System.Exit                      (exitFailure)
import           Types

type Inventory = M.Map Stem Int

data AppState = AppState
    { designs   :: [Design]
    , inventory :: Inventory
    }

type App = StateT AppState IO ()

-- HOF to continuously read from stdin and apply f to input until newline is
-- detected
untilNewline :: (ByteString -> App) -> App
untilNewline f =
    liftIO BS.getLine >>= \l -> unless (BS.null l) $ f l >> untilNewline f

readDesign :: ByteString -> App
readDesign line = case parseOnly parseDesign line of
    Left  err    -> liftIO $ print err >> exitFailure
    Right design -> modify (\s -> s { designs = design : designs s })

processStem :: ByteString -> App
processStem line = case parseOnly parseStem line of
    Left  err  -> liftIO $ print err >> exitFailure
    Right stem -> do
        modify $ \s -> s { inventory = insertWith (+) stem 1 (inventory s) }
        AppState designs inv <- get
        case map (arrangeBouquet inv) (filter (hasMinimumStock inv) designs) of
            (Just bouquet : _) -> do
                liftIO (print bouquet)
                modify (\s -> s { inventory = deductBouquet inv bouquet })
            _ -> return ()

arrangeBouquet :: Inventory -> Design -> Maybe Design
arrangeBouquet inventory design =
    case possibleArrangements inInventory (stemAmounts design) (capacity design)
    of
      (arrangement : _) -> Just (design { stemAmounts = arrangement })
      _                 -> Nothing
    where inInventory = gatherFrom inventory design

-- given a design return a list of stem amounts in the same order as the stems in the
-- design
gatherFrom :: Inventory -> Design -> [Int]
gatherFrom inv d = map (fromMaybe 0 . (`M.lookup` inv)) $ designStems d

-- given a completed bouquet, return a new map with the bouquet stems deducted from
-- inventory
deductBouquet :: Inventory -> Bouquet -> Inventory
deductBouquet inventory d@(Design _ _ stemAmounts _) =
  foldl (\inv (stem, amount) -> M.update (fn amount) stem inv) inventory toDeduct
  where
    toDeduct = zip (designStems d) (map maxAmount stemAmounts)
    fn used inStorage =
      if inStorage - used <= 0 then Nothing else Just $ inStorage - used

-- check if for a given design we have at least 1 of each stem, and the total in storage
-- is >= capacity of the bouquet
hasMinimumStock :: Inventory -> Design -> Bool
hasMinimumStock inventory design =
  sum (zipWith min inInventory maximum) >= capacity design
  where
    inInventory = gatherFrom inventory design
    maximum     = map maxAmount (stemAmounts design)

arrangementOption :: StemAmount -> [StemAmount]
arrangementOption (StemAmount 0 species) = []
arrangementOption s = s : arrangementOption s { maxAmount = maxAmount s - 1 }

possibleArrangements :: [Int] -> [StemAmount] -> Int -> [[StemAmount]]
possibleArrangements [] (_ : _) n = [[]]
possibleArrangements _  []      0 = [[]]
possibleArrangements _  []      p = []
possibleArrangements (inInventory : ys) (stem : xs) amount =
  [ o : z | o <- arrangementOption stemMax
          , z <- possibleArrangements ys xs (amount - maxAmount o)]
    where stemMax = stem { maxAmount = min inInventory (maxAmount stem) }

runApp :: App
runApp = untilNewline readDesign >> untilNewline processStem

main :: IO ()
main = void $ runStateT runApp (AppState [] M.empty)
