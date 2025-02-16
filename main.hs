-- main.hs
module Main where

import System.Exit (exitFailure)
import System.Environment (getArgs)
import System.IO ()
import System.Exit (die)
import Data.List (sortBy)
import Data.Char (intToDigit)

import Variable
import SudokuState
import Backtracking

import ForwardChecking_MRV ( solveForwardChecking )
import Mac ( solveMAC )  -- Βεβαιώσου ότι η συνάρτηση solveMAC εξάγεται από το module Mac

-- | Διαβάζει ένα αρχείο Sudoku (9 γραμμές με 9 χαρακτήρες) και επιστρέφει τις γραμμές του
parseSudokuFile :: FilePath -> IO [String]
parseSudokuFile fpath = do
    contents <- readFile fpath
    let sudokuLines = lines contents
    -- Έλεγχος: πρέπει να υπάρχουν ακριβώς 9 γραμμές
    if length sudokuLines /= 9
      then die "Error: The file must contain exactly 9 lines."
      else return ()
    -- Έλεγχος: κάθε γραμμή πρέπει να έχει ακριβώς 9 χαρακτήρες
    if any (\line -> length line /= 9) sudokuLines
      then die "Error: Each line must contain exactly 9 characters."
      else return ()
    return sudokuLines

-- | Αποθηκεύει το λύσιμο Sudoku σε αρχείο
writeSudokuToFile :: FilePath -> [String] -> IO ()
writeSudokuToFile filepath solution = do
    writeFile filepath (unlines solution)
    putStrLn $ "Solution saved to file: " ++ filepath

-- | Εκτυπώνει στην οθόνη ένα Sudoku (λίστα από γραμμές)
printSudoku :: [String] -> IO ()
printSudoku = mapM_ putStrLn

-- | Μετατρέπει την κατάσταση του Sudoku (SudokuState) σε λίστα από Strings
extractPuzzle :: SudokuState -> [String]
extractPuzzle state =
    let allVars = assigned state ++ unassigned state   -- Συνένωση όλων των μεταβλητών
        sortedVars = sortBy (\a b -> compare (varId a) (varId b)) allVars  -- Ταξινόμηση κατά varId
    in chunksOf 9 (map (intToDigit . head . domain) sortedVars)
  where
    -- Χωρίζει μια λίστα σε υπολίστες μήκους n (για τις γραμμές του Sudoku)
    chunksOf :: Int -> [a] -> [[a]]
    chunksOf _ [] = []
    chunksOf n xs = let (h, t) = splitAt n xs in h : chunksOf n t

-- | Η κύρια συνάρτηση. Διαβάζει τα ορίσματα και εκτελεί το πρόγραμμα.
main :: IO ()
main = do
    args <- getArgs
    case args of
         [method, sudokuFile] -> run method sudokuFile
         _ -> do
             putStrLn "Usage: main <method> <file_sudoku.txt>"
             putStrLn "Available methods:"
             putStrLn "  - naive  (Naive Backtracking and AllDifferent-2)"
             putStrLn "  - fc     (Forward Checking, MRV, Hidden Singles, Naked Pairs, and Failure Conditions)"
             putStrLn "  - mac    (Maintaining Arc Consistency - Bonus)"
             exitFailure

-- | Συνάρτηση που δέχεται τη μέθοδο επίλυσης και το όνομα του αρχείου και εκτελεί την επίλυση.
run :: String -> FilePath -> IO ()
run method sudokuFile = do
    -- 1. Διαβάζουμε το αρχικό Sudoku
    puzzle <- parseSudokuFile sudokuFile
    putStrLn "--------------------------"
    putStrLn "Initial Sudoku:"
    putStrLn "--------------------------"
    printSudoku puzzle

    -- 2. Δημιουργούμε τις μεταβλητές και την αρχική κατάσταση του CSP
    let variables = generateVariables puzzle
    let initialState = createSudokuState variables

    -- 3. Επιλέγουμε τη μέθοδο επίλυσης βάσει του ορίσματος
    let solution = case method of
                        "naive" -> solveNaive initialState
                        "fc"    -> solveForwardChecking initialState
                        "mac"   -> solveMAC initialState
                        _       -> error "Error: Unknown method! Try one of: naive, fc, fc-t, mac."

    putStrLn "--------------------------"
    putStrLn "Sudoku Solution:"
    putStrLn "--------------------------"
    
    -- 4. Εμφανίζουμε την λύση ή μήνυμα αποτυχίας
    case solution of
         Nothing -> putStrLn "No solution found."
         Just solvedState -> do
             let solvedPuzzle = extractPuzzle solvedState
             printSudoku solvedPuzzle
             writeSudokuToFile "sudoku_solution.txt" solvedPuzzle
