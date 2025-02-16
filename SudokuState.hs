-- SudokuState.hs
module SudokuState where

import Variable
import Data.List (partition)

data SudokuState = SudokuState
    { assigned   :: [Variable] -- Μεταβλητές με ανατεθείσα τιμή
    , unassigned :: [Variable] -- Μεταβλητές χωρίς ανατεθείσα τιμή
    }

-- Instance Eq για το SudokuState
instance Eq SudokuState where
    (SudokuState a1 u1) == (SudokuState a2 u2) =
        a1 == a2 && u1 == u2

instance Show SudokuState where
    show state =
        "Assigned Variables:\n" ++ unlines (map show (assigned state)) ++
        "Unassigned Variables:\n" ++ unlines (map show (unassigned state))


-- | Δημιουργεί το αρχικό SudokuState διαχωρίζοντας τις assigned και unassigned μεταβλητές
createSudokuState :: [Variable] -> SudokuState
createSudokuState vars =
    let (assignedVars, unassignedVars) = partition isAssigned vars
    in SudokuState assignedVars unassignedVars
  where
    isAssigned :: Variable -> Bool
    isAssigned var = length (domain var) == 1

-- Ενημερώνει το state αντικαθιστώντας τη μεταβλητή με το ίδιο varId με την νέα μεταβλητή.
updateVariable :: SudokuState -> Variable -> SudokuState
updateVariable state var =
  let allVars = assigned state ++ unassigned state
      updatedVars = map (\v -> if varId v == varId var then var else v) allVars
      (a, u) = partition (\v -> length (domain v) == 1) updatedVars
  in SudokuState a u


