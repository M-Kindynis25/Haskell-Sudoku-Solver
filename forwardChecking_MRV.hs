-- forwardChecking_MRV.hs
module ForwardChecking_MRV where

import Variable
import SudokuState
import SudokuTechniques

import Data.List (minimumBy)
import Data.Ord (comparing)

--------------------------------------------------------------------------------
-- Επιλογή Μεταβλητής με Minimum Remaining Values (MRV)
--
-- Η συνάρτηση selectVariableMRV επιλέγει τη μεταβλητή με το μικρότερο domain 
-- από τη λίστα των μη ανατεθειμένων μεταβλητών (unassigned). Αυτή η τεχνική 
-- επιτρέπει στον αλγόριθμο να επιλέγει πρώτα τις μεταβλητές με τις λιγότερες 
-- δυνατές τιμές, μειώνοντας έτσι τον παράγοντα διακλάδωσης (branching factor).
--------------------------------------------------------------------------------
selectVariableMRV :: SudokuState -> Variable
selectVariableMRV state = minimumBy (comparing (length . domain)) (unassigned state)

--------------------------------------------------------------------------------
-- Έλεγχος Εγκυρότητας Ανάθεσης
--
-- Η συνάρτηση isConsistent ελέγχει αν η ανάθεση μιας συγκεκριμένης τιμής σε μια 
-- μεταβλητή παραβιάζει κάποιους από τους περιορισμούς του Sudoku.
--
-- Για κάθε περιορισμό (constraint) της μεταβλητής, ελέγχεται αν η ανάθεση της 
-- προτεινόμενης τιμής παραβιάζει τους περιορισμούς σε σχέση με τις ήδη ανατεθειμένες
-- μεταβλητές (assigned state). 
--
-- Αν η σχετική μεταβλητή δεν έχει ακόμα ανατεθεί, η συνθήκη θεωρείται ικανοποιημένη.
--------------------------------------------------------------------------------
isConsistent :: Variable -> Int -> SudokuState -> Bool
isConsistent var value state = 
    all (\constraint ->
        case lookupVar (otherVarId constraint) (assigned state) of
            Nothing -> True      -- Αν η άλλη μεταβλητή δεν έχει ανατεθεί, δεν υπάρχει παραβίαση
            Just otherVar -> satisfies constraint value (head (domain otherVar))
        ) (constraints var)

--------------------------------------------------------------------------------
-- Forward Checking
--
-- Η συνάρτηση forwardCheck εφαρμόζει την τεχνική του Forward Checking:
--  - Για κάθε peer μεταβλητή (που σχετίζεται μέσω constraint με την τρέχουσα μεταβλητή),
--    αφαιρεί από το domain της την τιμή που μόλις ανατέθηκε στη μεταβλητή var.
--  - Αν το domain κάποιας peer μεταβλητής καταλήξει κενό, επιστρέφεται Nothing (αποτυχία).
--  - Διαφορετικά, επιστρέφεται η λίστα των ενημερωμένων μεταβλητών.
--------------------------------------------------------------------------------
forwardCheck :: Variable -> Int -> [Variable] -> Maybe [Variable]
forwardCheck var value unassignedVars = 
    let updatedVars = map (removeInconsistentValue var value) unassignedVars
    in if any (\v -> null (domain v)) updatedVars  -- Αν κάποια μεταβλητή έχει άδειο domain, επιστροφή Nothing
        then Nothing
        else Just updatedVars

--------------------------------------------------------------------------------
-- Αφαίρεση μη επιτρεπτής τιμής από το domain των peer μεταβλητών
--
-- Η removeInconsistentValue αφαιρεί την τιμή που μόλις ανατέθηκε από το domain 
-- όλων των peer μεταβλητών που σχετίζονται με την τρέχουσα μεταβλητή.
--
-- Η αποκοπή (pruning) αυτή βοηθά στη μείωση του χώρου αναζήτησης και οδηγεί 
-- σε πιο αποδοτική λύση.
--------------------------------------------------------------------------------
removeInconsistentValue :: Variable -> Int -> Variable -> Variable
removeInconsistentValue var value peer =
    if varId peer `elem` map otherVarId (constraints var)  -- Ελέγχει αν το peer σχετίζεται με το var
        then peer {domain = filter (/= value) (domain peer)} -- Αφαιρεί την τιμή value από το domain
        else peer

--------------------------------------------------------------------------------
-- Επίλυση του Sudoku με Forward Checking και MRV
--
-- Η solveForwardChecking χρησιμοποιεί το Forward Checking σε συνδυασμό με τη 
-- στρατηγική MRV για να επιλύσει το Sudoku.
--
-- Διαδικασία:
-- 1. Αρχικά, καλεί τη preprocessGrid ώστε να αφαιρεθούν οι μη έγκυρες τιμές 
--    από τα domains των μεταβλητών.
-- 2. Δημιουργείται μια νέα κατάσταση SudokuState με τις ενημερωμένες μεταβλητές.
-- 3. Καλείται η solveForwardChecking' που εκτελεί την πραγματική επίλυση.
--------------------------------------------------------------------------------
solveForwardChecking :: SudokuState -> Maybe SudokuState
solveForwardChecking state =
    -- let preprocessedVars = preprocessGrid (assigned state ++ unassigned state)  -- Μόνο εδώ γίνεται preprocessing
    --     newState = createSudokuState preprocessedVars  -- Δημιουργούμε νέο SudokuState με τα updated domains
    -- in solveForwardChecking' newState  -- Καλούμε την πραγματική επίλυση
    
    -- Εφαρμόζουμε πρώτα το preprocessing (Hidden Singles & Naked Pairs)
    let preprocessedVars = preprocessGrid (assigned state ++ unassigned state)
    in if detectFailure preprocessedVars 
         then Nothing  -- Αν εντοπιστεί failure, επιστρέφουμε αποτυχία
         else let newState = createSudokuState preprocessedVars
              in solveForwardChecking' newState
    
  where
    --------------------------------------------------------------------------------
    -- Αναδρομική Συνάρτηση Επίλυσης
    --
    -- Η solveForwardChecking' εκτελεί την πραγματική επίλυση με backtracking.
    -- 1. Αν δεν υπάρχουν μη ανατεθειμένες μεταβλητές, επιστρέφεται η τρέχουσα 
    --    κατάσταση ως λύση.
    -- 2. Επιλέγεται η μεταβλητή με τη στρατηγική MRV.
    -- 3. Δοκιμάζονται μία προς μία οι τιμές της μεταβλητής.
    -- 4. Εφαρμόζεται το Forward Checking:
    --    - Αν μετά το pruning κάποια μεταβλητή έχει κενό domain, γίνεται backtrack.
    -- 5. Η διαδικασία συνεχίζεται αναδρομικά μέχρι να βρεθεί μια λύση ή να 
    --    εξαντληθούν όλες οι πιθανές τιμές.
    --------------------------------------------------------------------------------
    solveForwardChecking' :: SudokuState -> Maybe SudokuState
    solveForwardChecking' state
        | null (unassigned state) = Just state  -- Αν όλες οι μεταβλητές έχουν ανατεθεί, έχουμε λύση
        | otherwise =
            let var = selectVariableMRV state  -- Επιλογή μεταβλητής με MRV
                possibleValues = domain var
            in tryValues var possibleValues state

    --------------------------------------------------------------------------------
    -- Δοκιμή Τιμών (Backtracking με Forward Checking)
    --
    -- Η tryValues επιχειρεί να αναθέσει μια τιμή στη μεταβλητή var.
    -- 1. Αν δεν υπάρχουν τιμές στο domain, επιστρέφεται αποτυχία (Nothing).
    -- 2. Αν η τιμή είναι συνεπής (isConsistent), δημιουργείται μια νέα κατάσταση
    --    και εκτελείται forward checking.
    -- 3. Αν μετά το forward checking κάποια μεταβλητή έχει κενό domain, δοκιμάζεται
    --    η επόμενη τιμή.
    -- 4. Αν βρεθεί έγκυρη ανάθεση, η διαδικασία συνεχίζεται αναδρομικά.
    --------------------------------------------------------------------------------
    tryValues :: Variable -> [Int] -> SudokuState -> Maybe SudokuState
    tryValues _ [] _ = Nothing  -- Καμία τιμή δεν είναι έγκυρη
    tryValues var (v:vs) state =
        if isConsistent var v state
        then 
            let newVar = var { domain = [v] }                        
                newAssigned = newVar : assigned state              
                newUnassigned = filter (\x -> varId x /= varId var) (unassigned state)
                tentativeState = SudokuState newAssigned newUnassigned
                 -- Επαναπροεπεξεργασία μετά από την ανάθεση
                preprocessed = preprocessGrid (assigned tentativeState ++ unassigned tentativeState)

            in if detectFailure preprocessed 
                then tryValues var vs state  -- Αποτυχία: δοκιμάζουμε την επόμενη τιμή
                else 
                    let newState = createSudokuState preprocessed
                    in case solveForwardChecking' newState of
                        Nothing -> tryValues var vs state -- Αν η τρέχουσα επιλογή απέτυχε, δοκιμάζει την επόμενη τιμή
                        solution -> solution  
        else tryValues var vs state
