module Main where

import           Control.Applicative
import           Control.Monad                    (unless, void)
import           Control.Monad.Cont               (MonadIO (liftIO))
import           Control.Monad.State              (StateT, get, liftIO, modify,
                                                   runStateT)
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString                  as BS (getLine, null)
import           Data.ByteString.Char8            (ByteString)
import qualified Data.Map                         as M (Map, empty, fromList,
                                                        lookup, unionWith,
                                                        update)
import           Data.Map.Strict                  (insertWith)
import           Data.Maybe                       (fromMaybe, mapMaybe)
import           Parser                           (parseDesign, parseStem)
import           System.Exit                      (exitFailure)
import           Types

type Inventory = M.Map Stem Int

data AppState = AppState [Design] Inventory

type App = StateT AppState IO ()

untilNewline :: (ByteString -> App) -> App
untilNewline f =
  liftIO BS.getLine >>= \l -> unless (BS.null l) $ f l >> untilNewline f

readDesign :: ByteString -> App
readDesign line = case parseOnly parseDesign line of
  Left err     -> liftIO $ print err >> exitFailure
  Right design -> modify (\(AppState ds i) -> AppState (design:ds) i )

deductBouquet :: Inventory -> Bouquet -> Inventory
deductBouquet inventory design =
  M.unionWith (-) inventory toDeduct
  where
    toDeduct = M.fromList $ zip (designStems design) (map maxAmount $ stemAmounts design)

processStem :: ByteString -> App
processStem line = case parseOnly parseStem line of
  Left err -> liftIO $ print err >> exitFailure
  Right stem -> do
    modify $ \(AppState d i) -> AppState d $ insertWith (+) stem 1 i
    AppState designs inv <- get
    case mapMaybe (arrangeBouquet inv) designs of
      (bouquet : _) -> do
        liftIO (print bouquet)
        modify (\(AppState d i) -> AppState d $ deductBouquet i bouquet)
      _ -> return ()

arrangeBouquet :: Inventory -> Design -> Maybe Bouquet
arrangeBouquet inventory design
  | null possibleArrangements = Nothing
  | otherwise = Just $ design {stemAmounts = head possibleArrangements}
  where
    possibleArrangements = (arrangements . availableStems inventory) design (capacity design)

availableStems :: Inventory -> Design -> [StemAmount]
availableStems inv d = zipWith minAvail (stemAmounts d) inInventory
  where
    inInventory = map (fromMaybe 0 . (`M.lookup` inv)) $ designStems d
    minAvail stem inInv = stem {maxAmount = min (maxAmount stem) inInv}

arrangements :: [StemAmount] -> Int -> [[StemAmount]]
arrangements [] 0 = [[]]
arrangements [] p = []
arrangements ((StemAmount 0 _) : _) _ = []
arrangements (stem@(StemAmount m _) : xs) amount =
  [stem {maxAmount = o} : z | o <- [m, (m -1) .. 1], z <- arrangements xs (amount - o)]

main :: IO ()
main = void $ runStateT app (AppState [] M.empty)
  where app = untilNewline readDesign >> untilNewline processStem
