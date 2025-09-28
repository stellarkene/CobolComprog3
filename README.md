1. Naming & Structure
Use clear, descriptive paragraph and variable names.
DO: ADD-STUDENT
DONT: P1 or DOIT
Group related data in 01/05 levels and use OCCURS for arrays.
Keep each paragraph focused on one task (like functions).


2. Indentation & Layout
Indent one level for DISPLAY/ACCEPT under PERFORM/EVALUATE.
One paragraph per block of logic, label it clearly:

 PROCEDURE DIVISION.
MAIN-PARA.
    PERFORM MENU-PARA
    STOP RUN.
    
End paragraphs with EXIT. (optional but clear).


3. EVALUATE / PERFORM Blocks
Always use END-EVALUATE and END-PERFORM instead of a period in the middle.
Period at the very end of your paragraph/program only.


4. Comments
Use * for comments:
 * Accept student name from user
ACCEPT SI-NAME(1)


Git guidelines
1. Atomic Commits
One logical change per commit:
DO: “Add menu option for Add Student”
DONT: “Add stuff”
Don’t mix formatting changes with logic changes.


2. Commit Message Format
Use imperative mood:

“ Add input validation to Add Student menu “
“Fix array subscript error in STUDENTS”
“Refactor menu loop for clarity”

If needed, add a short body explaining why.


3. Reviews
Before merging, quickly check:

- Code compiles.
- No obvious style violations.
- Each commit message makes sense.
