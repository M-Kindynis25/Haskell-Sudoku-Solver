-- Variable.hs
module Variable where

import Data.Char (digitToInt)
import Data.List (find)
import Data.List (nub)

data Constraint = Cons Int (Int -> Int -> Bool)

-- Instance για εκτύπωση της δομής Constraint
instance Show Constraint where
    show (Cons cid _) = show cid ++ ", "

-- Επιστρέφει το ID της μεταβλητής που σχετίζεται με έναν περιορισμό (Constraint).
otherVarId :: Constraint -> Int
otherVarId (Cons vid _) = vid

-- Ελέγχει αν δύο τιμές ικανοποιούν τον περιορισμό.
satisfies :: Constraint -> Int -> Int -> Bool
satisfies (Cons _ constraintFunc) x y = constraintFunc x y

-- Αναζητά μια μεταβλητή με συγκεκριμένο ID μέσα σε μια λίστα μεταβλητών.
-- Αν βρεθεί, επιστρέφεται `Just Variable`, αλλιώς `Nothing`.
lookupVar :: Int -> [Variable] -> Maybe Variable
lookupVar vid vars = find (\v -> varId v == vid) vars


data Variable = Var 
    { varId       :: Int          -- Μοναδικό ID της μεταβλητής
    , domain      :: [Int]        -- Πιθανές τιμές
    , constraints :: [Constraint] -- Λίστα περιορισμών
    }

-- Instance για σύγκριση μεταβλητών
instance Eq Variable where
    (Var id1 _ _) == (Var id2 _ _) = id1 == id2

-- Instance για εκτύπωση της δομής Variable
instance Show Variable where
    show (Var vid dom cons) = 
        "Variable ID: " ++ show vid ++ ", Domain: " ++ show dom ++ ", Constraints: [" ++ show cons ++ "]"




-- | Δημιουργεί μια λίστα από Variables με IDs, Domains και Constraints
generateVariables :: [String] -> [Variable]
generateVariables lines =
    addConstraints $ zipWith createVariable [0..80] (concat lines)
  where
    createVariable :: Int -> Char -> Variable
    createVariable idx ch =
        case ch of
            '.' -> Var idx [1..9] []  -- Άδειο κελί
            _   -> Var idx [digitToInt ch] [] -- Συμπληρωμένο κελί

-- Η συνάρτηση preprocessGrid λαμβάνει ως είσοδο μια λίστα μεταβλητών και επιστρέφει
-- μια ενημερωμένη λίστα, όπου για κάθε μεταβλητή που είναι ήδη ανατεθειμένη, η τιμή
-- της αφαιρείται από τα domains των γειτονικών (peers).
preprocessGrid :: [Variable] -> [Variable]
preprocessGrid vars = foldl pruneForAssigned vars assignedVars
  where
    -- Επιλέγουμε τις μεταβλητές που είναι ήδη ανατεθειμένες (domain με μοναδική τιμή)
    assignedVars :: [Variable]
    assignedVars = filter (\v -> length (domain v) == 1) vars

    -- Για κάθε μεταβλητή v που είναι ήδη ανατεθειμένη, ενημερώνουμε τις μεταβλητές
    -- στο πλέγμα αφαιρώντας την τιμή της από τα domains των μεταβλητών που βρίσκονται
    -- στα ίδια units (γραμμή, στήλη, υποπλέγμα).
    pruneForAssigned :: [Variable] -> Variable -> [Variable]
    pruneForAssigned currentVars v =
      let assignedVal = head (domain v)
          -- Βρίσκουμε τα peers της μεταβλητής v χρησιμοποιώντας την υλοποίηση που έχετε ήδη
          peers = findPeers (varId v) currentVars
      in map (\peer ->
              -- Ελέγχουμε αν το peer είναι στη λίστα των peers της v
              if varId peer `elem` map varId peers
                then 
                  -- Αφαιρούμε την τιμή assignedVal από το domain του peer
                  peer { domain = filter (/= assignedVal) (domain peer) }
                else peer
             ) currentVars


-- | Προσθέτει AllDifferent-2 constraints σε κάθε μεταβλητή
addConstraints :: [Variable] -> [Variable]
addConstraints vars =
    map addVarConstraints vars
  where
    addVarConstraints :: Variable -> Variable
    addVarConstraints var =
        let peers = findPeers (varId var) vars
            newCons = map (\peer -> Cons (varId peer) (/=)) peers
        in var { constraints = newCons }

-- | Βρίσκει όλα τα peers (σε γραμμή, στήλη, υποπλέγμα) μιας μεταβλητής
findPeers :: Int -> [Variable] -> [Variable]
findPeers idx vars =
    let (row, col, box) = getUnitIndices idx
        peersRow = [v | v <- vars, getRow v == row, varId v /= idx]
        peersCol = [v | v <- vars, getCol v == col, varId v /= idx]
        peersBox = [v | v <- vars, getBox v == box, varId v /= idx]
    in nub (peersRow ++ peersCol ++ peersBox)

-- | Λαμβάνει το row, column και box ενός μεταβλητού με βάση το ID του
getUnitIndices :: Int -> (Int, Int, Int)
getUnitIndices idx =
    let row = idx `div` 9
        col = idx `mod` 9
        box = (row `div` 3) * 3 + (col `div` 3)
    in (row, col, box)

-- | Βρίσκει το row μιας μεταβλητής
getRow :: Variable -> Int
getRow var = varId var `div` 9

-- | Βρίσκει το column μιας μεταβλητής
getCol :: Variable -> Int
getCol var = varId var `mod` 9

-- | Βρίσκει το box μιας μεταβλητής
getBox :: Variable -> Int
getBox var =
    let row = getRow var
        col = getCol var
    in (row `div` 3) * 3 + (col `div` 3)
