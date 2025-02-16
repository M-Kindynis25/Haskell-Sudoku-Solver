-- backtracking.hs
module Backtracking where

import Variable
import SudokuState

--------------------------------------------------------------------------------
-- Επιλογή της πρώτης μη ανατεθειμένης μεταβλητής
--
-- Η συνάρτηση selectUnassignedVariable επιλέγει την πρώτη μεταβλητή από
-- τη λίστα των μεταβλητών που δεν έχουν ακόμη ανατεθεί (δηλαδή, βρίσκονται
-- στο πεδίο unassigned της κατάστασης SudokuState). Αυτή η επιλογή είναι απλή
-- (π.χ., χωρίς χρήση συγκεκριμένων heuristics όπως το MRV), αλλά αρκετά
-- αποτελεσματική για μικρά ή σχετικά απλά προβλήματα Sudoku.
--------------------------------------------------------------------------------
selectUnassignedVariable :: SudokuState -> Variable
selectUnassignedVariable state = head (unassigned state)

--------------------------------------------------------------------------------
-- Έλεγχος Ολοκληρωμένου Assignment: allConstraintsSatisfied
--
-- Η συνάρτηση allConstraintsSatisfied χρησιμοποιείται όταν έχουν ανατεθεί όλες
-- οι μεταβλητές (δηλαδή, η λίστα unassigned είναι κενή). Ελέγχει αν το πλήρες
-- assignment ικανοποιεί όλους τους περιορισμούς.
--
-- Για κάθε μεταβλητή που έχει ανατεθεί (στη λίστα assigned), η συνάρτηση
-- ελέγχει όλους τους περιορισμούς που αυτή έχει (που ορίζονται ως λίστα
-- constraints). Κάθε περιορισμός αφορά ένα "peer" (συναφές κελί στη σειρά, στήλη
-- ή box) και εφαρμόζεται χρησιμοποιώντας τη συνάρτηση του περιορισμού.
--------------------------------------------------------------------------------
allConstraintsSatisfied :: SudokuState -> Bool
allConstraintsSatisfied state =
    -- Ελέγχουμε για κάθε ανατεθειμένη μεταβλητή (var) ότι όλοι οι περιορισμοί της
    -- ικανοποιούνται.
    all (\var -> all (checkConstraint var) (constraints var)) (assigned state)
  where
    -- | Η συνάρτηση checkConstraint παίρνει μια μεταβλητή και έναν περιορισμό, και
    -- ελέγχει αν ο περιορισμός ικανοποιείται μεταξύ της τρέχουσας μεταβλητής και
    -- του "peer" που καθορίζεται από το ID του περιορισμού.
    checkConstraint :: Variable -> Constraint -> Bool
    checkConstraint var (Cons peerId constraintFunc) =
        case lookupVar peerId (assigned state) of
            -- Αν υπάρχει η μεταβλητή του περιορισμού στο πλήρες assignment, τότε
            -- εφαρμόζουμε τη συνάρτηση του περιορισμού (constraintFunc) στις τιμές
            -- των δύο μεταβλητών. Επειδή κάθε μεταβλητή έχει πλέον μόνο μία τιμή στο
            -- domain (δηλαδή, ένα singleton), χρησιμοποιούμε το head για να πάρουμε αυτήν την τιμή.
            Just peerVar -> constraintFunc (head (domain var)) (head (domain peerVar))
            -- Σε ένα πλήρες assignment, αυτή η περίπτωση δεν πρέπει να συμβαίνει.
            Nothing -> False  

--------------------------------------------------------------------------------
-- Λύση Sudoku με Naive Backtracking: solveNaive
--
-- Η συνάρτηση solveNaive επιχειρεί να επιλύσει το πρόβλημα του Sudoku χρησιμοποιώντας
-- μια απλή αναδρομική προσέγγιση backtracking. Σε αυτή την υλοποίηση, ο έλεγχος
-- των περιορισμών πραγματοποιείται μόνο αφού έχουν ανατεθεί όλες οι μεταβλητές.
--
-- Διαδικασία:
-- 1. Αν δεν υπάρχουν μη ανατεθειμένες μεταβλητές (δηλαδή το assignment είναι πλήρες),
--    τότε καλείται η allConstraintsSatisfied για να επιβεβαιωθεί ότι το assignment
--    ικανοποιεί όλους τους περιορισμούς. Αν ικανοποιεί, επιστρέφουμε την κατάσταση ως λύση.
--
-- 2. Αν υπάρχουν μη ανατεθειμένες μεταβλητές, επιλέγουμε την πρώτη από αυτές και
--    προσπαθούμε αναδρομικά να της αναθέσουμε κάθε τιμή από το domain της.
--
-- 3. Για κάθε δοκιμασμένη τιμή:
--    - Δημιουργούμε ένα νέο αντικείμενο μεταβλητής με το domain περιορισμένο στη δοκιμαστέα τιμή.
--    - Ενημερώνουμε την κατάσταση (SudokuState) προσθέτοντας αυτή τη μεταβλητή στη λίστα assigned,
--      και αφαιρώντας την από την λίστα unassigned.
--    - Καλούμε αναδρομικά τη solveNaive με τη νέα κατάσταση.
--    - Αν η αναδρομική κλήση επιστρέψει Nothing, δοκιμάζουμε την επόμενη τιμή.
--    - Αν βρεθεί λύση, την επιστρέφουμε.
--------------------------------------------------------------------------------
solveNaive :: SudokuState -> Maybe SudokuState
solveNaive state
    -- Βάση αναδρομής: Ελέγχουμε αν δεν υπάρχουν μη ανατεθειμένες μεταβλητές.
    | null (unassigned state) =
          if allConstraintsSatisfied state 
             then Just state   -- Επιστρέφουμε την κατάσταση ως λύση αν όλοι οι περιορισμοί ικανοποιούνται.
             else Nothing      -- Διαφορετικά, το πλήρες assignment δεν είναι έγκυρο.
    | otherwise =
         -- Επιλογή της πρώτης μη ανατεθειμένης μεταβλητής.
         let var = selectUnassignedVariable state
             possibleValues = domain var  -- Λαμβάνουμε το σύνολο πιθανών τιμών για τη μεταβλητή.
         in tryValues var possibleValues state  -- Προσπαθούμε να αναθέσουμε κάθε πιθανή τιμή.
  where
    -- | Η βοηθητική συνάρτηση tryValues δοκιμάζει κάθε τιμή για τη μεταβλητή var.
    -- Αν εξαντληθούν όλες οι τιμές χωρίς επιτυχία, επιστρέφει Nothing.
    tryValues :: Variable -> [Int] -> SudokuState -> Maybe SudokuState
    tryValues _ [] _ = Nothing
    tryValues var (v:vs) state =
      -- Δημιουργούμε ένα νέο αντικείμενο μεταβλητής με τιμή v (δηλαδή, το domain γίνεται [v]).
      let newVar = var { domain = [v] }
          -- Ενημερώνουμε τη λίστα των ανατεθειμένων μεταβλητών προσθέτοντας το newVar.
          newAssigned = newVar : assigned state
          -- Ενημερώνουμε τη λίστα των μη ανατεθειμένων μεταβλητών αφαιρώντας τη μεταβλητή που μόλις ανατέθηκε.
          newUnassigned = filter (\x -> varId x /= varId var) (unassigned state)
          -- Δημιουργούμε τη νέα κατάσταση του Sudoku με το ανανεωμένο assignment.
          newState = SudokuState newAssigned newUnassigned
      in case solveNaive newState of
          -- Αν η αναδρομική κλήση με τη νέα κατάσταση αποτύχει (δηλ. δεν ικανοποιούνται οι περιορισμοί στο τελικό assignment),
          -- τότε δοκιμάζουμε την επόμενη πιθανή τιμή για τη μεταβλητή.
          Nothing -> tryValues var vs state
          -- Αν βρεθεί λύση, την επιστρέφουμε.
          solution -> solution

    






-- Για κάθε τιμή του domain της μεταβλητής , ελεγχει αν η ανάθεση αυτη είναι έγκυρη
isConsistent :: Variable -> Int -> SudokuState -> Bool
isConsistent var value state = 
    -- Για κάθε constraint στη μεταβλητή
    all (\constraint ->
        case lookupVar (otherVarId constraint) (assigned state) of
            Nothing -> True      -- Αν η άλλη μεταβλητή δεν έχει ανατεθεί, δεν υπάρχει παραβίαση
            Just otherVar -> satisfies constraint value (head (domain otherVar))
        ) (constraints var)
-- 
solveNaive2 :: SudokuState -> Maybe SudokuState
solveNaive2 state
    | null (unassigned state) = Just state  -- Ολες οι μεταβλητές έχουν ανατεθεί
    | otherwise = 
        let var = selectUnassignedVariable state
            possibleValues = domain var
        in tryValues var possibleValues state
    where 
        tryValues _ [] _ = Nothing  -- Καμία τιμή δεν βρέθηκε
        tryValues var (v:vs) state = 
            if isConsistent var v state
            then 
                let newVar = var { domain = [v] }                       -- Δημιουργεί νέα μεταβλητή με τιμή v
                    newAssigned = newVar : assigned state               -- Προσθέτει τη μεταβλητή στις assigned
                    newUnassigned = filter (\x -> varId x /= varId var) (unassigned state) -- Αφαιρεί από τις μη ανατεθειμένες
                    newState = SudokuState newAssigned newUnassigned    -- Δημιουργεί το νέο state
                in case solveNaive2 newState of
                    Nothing -> tryValues var vs state   -- Αν η τρέχουσα επιλογή απέτυχε, δοκιμάζει την επόμενη τιμή
                    solution -> solution                -- Αν βρήκε λύση, την επιστρέφει
            else tryValues var vs state