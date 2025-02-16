-- SudokuTechniques.hs
module SudokuTechniques where

import Variable
import Data.List (groupBy, sortBy, nub, (\\))
import Data.Ord (comparing)
import Data.Maybe (fromMaybe)

--------------------------------------------------------------------------------
-- 1️ Hidden Singles (Κρυφά Μοναδικά)
--
-- Αν μια τιμή εμφανίζεται μόνο σε ένα κελί μιας μονάδας (γραμμή, στήλη, box),
-- τότε αυτή η μεταβλητή **υποχρεωτικά** πρέπει να πάρει αυτήν την τιμή.
--------------------------------------------------------------------------------
applyHiddenSingles :: [Variable] -> [Variable]
applyHiddenSingles vars = map assignIfHiddenSingle vars
  where
    -- Εντοπισμός όλων των μοναδικών τιμών σε κάθε μονάδα (γραμμή, στήλη, box)
    hiddenSingles = concatMap findHiddenSingles (groupUnits vars)

    -- Αν μια μεταβλητή περιέχεται στα Hidden Singles, αναθέτουμε τη μοναδική τιμή της
    assignIfHiddenSingle var =
      case lookup (varId var) hiddenSingles of
        Just value -> var { domain = [value] }
        Nothing    -> var

    -- Επιστρέφει έναν χάρτη (varId -> μοναδική τιμή)
    findHiddenSingles :: [Variable] -> [(Int, Int)]
    findHiddenSingles unit =
      let allValues = concatMap domain unit
          uniqueValues = nub [ v | v <- allValues, length (filter (elem v . domain) unit) == 1 ]
      in [(varId var, v) | var <- unit, [v] <- [filter (`elem` uniqueValues) (domain var)]]

--------------------------------------------------------------------------------
-- 2️ Naked Pairs (Γυμνά Ζευγάρια)
--
-- Αν δύο κελιά της ίδιας μονάδας (γραμμή, στήλη, box) έχουν **το ίδιο domain**
-- και **περιέχουν ακριβώς δύο τιμές**, τότε αυτές οι τιμές μπορούν να αφαιρεθούν
-- από τα υπόλοιπα κελιά της μονάδας.
--------------------------------------------------------------------------------
applyNakedPairs :: [Variable] -> [Variable]
applyNakedPairs vars = map removeNakedPairs vars
  where
    -- Εντοπίζουμε τα Naked Pairs σε κάθε μονάδα (γραμμή, στήλη, box)
    nakedPairs = concatMap findNakedPairs (groupUnits vars)

    -- Αφαιρούμε τις τιμές των Naked Pairs από τα υπόλοιπα κελιά
    removeNakedPairs var =
        let conflicts = concat [ p | (pos, p) <- nakedPairs, pos /= varId var ]
        in if null conflicts then var else var { domain = domain var \\ conflicts }

    -- Επιστρέφει έναν χάρτη (varId -> ζευγάρι τιμών)
    findNakedPairs :: [Variable] -> [(Int, [Int])]
    findNakedPairs unit =
      let pairs = [ (v1, v2) 
                  | v1 <- unit, v2 <- unit, varId v1 < varId v2,
                    domain v1 == domain v2, length (domain v1) == 2 ]
          pairValues = nub [ domain v1 | (v1, _) <- pairs ]
      in [ (varId v, values) 
         | values <- pairValues, v <- unit, domain v /= values ]

--------------------------------------------------------------------------------
-- 3️ Συνθήκες Αποτυχίας (Failure Conditions)
--
-- Μια κατάσταση θεωρείται αποτυχία αν υπάρχει μονάδα όπου **n μεταβλητές**
-- έχουν **το ίδιο domain**, αλλά **|domain| < n**, καθώς αυτό σημαίνει ότι δεν
-- είναι δυνατή η ανάθεση διαφορετικών τιμών στις μεταβλητές.
--------------------------------------------------------------------------------
detectFailure :: [Variable] -> Bool
detectFailure vars = any checkUnitFailure (groupUnits vars)
  where
    checkUnitFailure :: [Variable] -> Bool
    checkUnitFailure unit =
      let grouped = groupBy (\v1 v2 -> domain v1 == domain v2)
                             (sortBy (comparing domain) unit)
      in any (\grp -> length grp > length (domain (head grp))) grouped

--------------------------------------------------------------------------------
-- Βοηθητική Συνάρτηση: Ομαδοποίηση Μεταβλητών σε Μονάδες (γραμμές, στήλες, κουτιά)
--
-- Χωρίζει τις μεταβλητές του Sudoku σε ομάδες που αντιστοιχούν στις μονάδες:
--  - 9 γραμμές (row groups)
--  - 9 στήλες (column groups)
--  - 9 υποπλέγματα 3x3 (box groups)
--------------------------------------------------------------------------------
groupUnits :: [Variable] -> [[Variable]]
groupUnits vars = [ findRow i vars | i <- [0..8] ]
               ++ [ findCol i vars | i <- [0..8] ]
               ++ [ findBox i vars | i <- [0..8] ]
  where
    findRow row vars = filter (\v -> getRow v == row) vars
    findCol col vars = filter (\v -> getCol v == col) vars
    findBox box vars = filter (\v -> getBox v == box) vars
