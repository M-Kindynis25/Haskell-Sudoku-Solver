# Sudoku Solver in Haskell

## Περιγραφή

Το παρόν project υλοποιεί έναν **Sudoku Solver** σε **Haskell**, βασισμένο στη μοντελοποίηση του Sudoku ως **Πρόβλημα Ικανοποίησης Περιορισμών** (Constraint Satisfaction Problem - CSP). Κάθε κελί του Sudoku θεωρείται μεταβλητή (variable) με πιθανές τιμές (domain) τους ακέραιους από το 1 έως το 9, ενώ οι περιορισμοί (constraints) μεταξύ των μεταβλητών επιβάλλουν ότι οι τιμές πρέπει να διαφέρουν σε κάθε γραμμή, στήλη και υποπλέγμα 3×3.

### Στόχος

- **Ανάγνωση** ενός αρχείου κειμένου `.txt` που περιέχει ένα πλέγμα Sudoku (9 γραμμές × 9 χαρακτήρες η καθεμία).
- **Μετατροπή** του Sudoku σε CSP, ορίζοντας:
  - **Μεταβλητές** με μοναδικά IDs και domains.
  - **Δυαδικούς περιορισμούς** (AllDifferent-2) ανάμεσα σε ζεύγη κελιών που ανήκουν στην ίδια γραμμή, στήλη ή υποπλέγμα (3×3).
- **Επίλυση** του Sudoku με διάφορες μεθόδους:
  - **Naive Backtracking** (AllDifferent-2).
  - **Forward Checking + MRV** (με Hidden Singles, Naked Pairs και  Συνθήκες Αποτυχίας).
  - **Maintaining Arc Consistency (MAC)** - αν υλοποιηθεί (εδώ επιστρέφει Nothing).

Μόλις βρεθεί λύση, εκτυπώνεται στην οθόνη και αποθηκεύεται προαιρετικά σε αρχείο (π.χ. `sudoku_solution.txt`).

---

## Δομή Αρχείων

1. **`Variable.hs`**  
   - Ορισμός του τύπου `Variable`:
     - `varId :: Int`  
     - `domain :: [Int]`  
     - `constraints :: [Constraint]`
   - Διαχείριση περιορισμών (AllDifferent-2), συναρτήσεις βοηθητικές (`lookupVar`, `satisfies`, κ.λπ.).
   - Συνάρτηση `generateVariables` που φτιάχνει όλες τις μεταβλητές (9×9).
   - Συνάρτηση `addConstraints` που προσθέτει τους AllDifferent-2 περιορισμούς.

2. **`SudokuState.hs`**  
   - Ορισμός του τύπου `SudokuState` με:
     - `assigned :: [Variable]`
     - `unassigned :: [Variable]`
   - Συνάρτηση `createSudokuState` για να διαχωρίζει τις μεταβλητές σε assigned/unassigned (αν ένα κελί έχει μοναδική τιμή στο domain, θεωρείται assigned).
   - Λοιπές συναρτήσεις διαχείρισης της κατάστασης.

3. **`Backtracking.hs`**  
   - **Naive Backtracking** αλγόριθμος:
     - `selectUnassignedVariable`: Επιλογή της πρώτης μη ανατεθειμένης μεταβλητής.
     - `solveNaive`: Αναδρομική αναζήτηση (δοκιμάζει τιμές στα κενά κελιά, με έλεγχο των περιορισμών).

4. **`ForwardChecking_MRV.hs`**  
   - **Forward Checking** με **MRV**:
     - `selectVariableMRV`: Διαλέγει μεταβλητή με το μικρότερο domain.
     - `forwardCheck`, `removeInconsistentValue`: Αφαιρεί τιμές από τα domains των peer μεταβλητών.
     - `solveForwardChecking`: Συνδυάζει backtracking + forward checking.  
     - Προσθήκη/ενσωμάτωση *SudokuTechniques* (hidden singles, naked pairs) εφόσον υλοποιηθούν.

5. **`Mac.hs`**  
   - Υλοποίηση του **Maintaining Arc Consistency (MAC)** (εδώ είναι stub και γυρνάει `Nothing`).

6. **`main.hs`**  
   - Διαβάζει ορίσματα γραμμής εντολών (`method` και `sudoku_file.txt`).
   - Φορτώνει το αρχείο Sudoku και ελέγχει ότι έχει 9 γραμμές × 9 χαρακτήρες.
   - Φτιάχνει το `SudokuState`.
   - Ανάλογα με το `method`, καλεί την αντίστοιχη συνάρτηση επίλυσης (`solveNaive`, `solveForwardChecking`, `solveMAC`).
   - Εκτυπώνει το αρχικό Sudoku, τη λύση (αν υπάρχει), και αποθηκεύει τη λύση σε αρχείο `sudoku_solution.txt`.

---

## Οδηγίες Χρήσης

1. **Μεταγλώττιση**  
   Μεταγλωττίζουμε με τον GHC (π.χ. `ghc`):
   ```
   ghc main.hs -o sudoku-solver
   ```

2. **Εκτέλεση στο τερματικό**
   Η εκτέλεση του προγράμματος γίνεται δίνοντας ως ορίσματα τη μέθοδο επίλυσης και το αρχείο Sudoku:
   ```
   ./sudoku-solver <method> <file_sudoku.txt>
   ```
   Όπου:
    - `<method>`: Η μέθοδος επίλυσης που θα χρησιμοποιηθεί.
      - naive  (Naive Backtracking and AllDifferent-2)
      - fc     (Forward Checking, MRV, Hidden Singles, Naked Pairs, and Failure Conditions)
      - mac    (Maintaining Arc Consistency - Bonus)
    - `<file_sudoku.txt>`: Το αρχείο που περιέχει το πλέγμα Sudoku προς επίλυση.

3. **Εκτέλεση στο GHCi**
   Για εκτέλεση μέσα από το περιβάλλον του GHCi, κάντε τα εξής:
   ```
   ghci main.hs
   :main fc sudoku.txt
   ```

