       IDENTIFICATION DIVISION.
       PROGRAM-ID. EmployeeManagementSystem.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EmployeeFile 
               ASSIGN TO 'employees.dat'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.
               
           SELECT TempFile ASSIGN TO 'temp.dat'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.
       
       DATA DIVISION.
       FILE SECTION.
       FD  EmployeeFile.
       01  EmployeeRecord.
           05 EmployeeID      PIC 9(5).
           05 EmployeeName    PIC X(30).
           05 EmployeeGrade   PIC X(5).
           05 EmployeeSalary  PIC 9(5)V99.
       
       FD TempFile.
       01 TempRecord.
           05 TempID         PIC 9(5).
           05 TempName       PIC X(30).
           05 TempGrade      PIC X(5).
           05 TempSalary     PIC 9(5)V99.
       
       WORKING-STORAGE SECTION.
       01 WS-EmployeeID      PIC 9(5).
       01 WS-EmployeeName    PIC X(30).
       01 WS-EmployeeGrade   PIC X(5).
       01 WS-EmployeeSalary  PIC 9(5)V99.
       01 WS-Continue        PIC X VALUE 'Y'.
       01 WS-Choice          PIC X.
       01 WS-File-Status     PIC XX.
       
       PROCEDURE DIVISION.
           PERFORM UNTIL WS-Continue = 'N'
               DISPLAY '--------Employee Management System--------'
               DISPLAY '1. Create'
               DISPLAY '2. Read'
               DISPLAY '3. Update'
               DISPLAY '4. Delete'
               DISPLAY '------------------------------------------'
               DISPLAY 'Enter your choice: ' WITH NO ADVANCING
               ACCEPT WS-Choice
               EVALUATE WS-Choice
                   WHEN '1'
                       PERFORM CreateEmployee
                   WHEN '2'
                       PERFORM ReadEmployees
                   WHEN '3'
                       PERFORM UpdateEmployee
                   WHEN '4'
                       PERFORM DeleteEmployee
                   WHEN OTHER
                       DISPLAY 'Invalid choice'
               END-EVALUATE
               DISPLAY 'Do you want to continue (Y/N)?: ' 
               WITH NO ADVANCING
               ACCEPT WS-Continue
           END-PERFORM
           STOP RUN.
       
       CreateEmployee.
           OPEN OUTPUT EmployeeFile
           IF WS-File-Status NOT = '00'
               DISPLAY 'Error opening EmployeeFile for extend, status: ' 
                   WS-File-Status
               STOP RUN
           END-IF
           DISPLAY '------------------------------------------'.
           DISPLAY 'Enter Employee ID: ' WITH NO ADVANCING.
           ACCEPT WS-EmployeeID.
           DISPLAY 'Enter Employee Name: ' WITH NO ADVANCING.
           ACCEPT WS-EmployeeName.
           DISPLAY 'Enter Employee Grade: ' WITH NO ADVANCING.
           ACCEPT WS-EmployeeGrade.
           DISPLAY 'Enter Employee Salary: ' WITH NO ADVANCING.
           ACCEPT WS-EmployeeSalary.
           DISPLAY '------------------------------------------'.

           MOVE WS-EmployeeID TO EmployeeID
           MOVE WS-EmployeeName TO EmployeeName
           MOVE WS-EmployeeGrade TO EmployeeGrade
           MOVE WS-EmployeeSalary TO EmployeeSalary
           WRITE EmployeeRecord

           IF WS-File-Status NOT = '00'
               DISPLAY 'Error writing to EmployeeFile, status: ' 
                   WS-File-Status
           END-IF
           CLOSE EmployeeFile.

           DISPLAY 'Employee added successfully'.
       
       ReadEmployees.
           OPEN INPUT EmployeeFile
           IF WS-File-Status NOT = '00'
               DISPLAY 'Error opening EmployeeFile for input'
               STOP RUN
           END-IF
           
           DISPLAY '------------------------------------------'.
           PERFORM UNTIL WS-File-Status = '10'
               READ EmployeeFile INTO EmployeeRecord
               AT END
                   EXIT PERFORM
               NOT AT END
                   DISPLAY EmployeeID SPACE EmployeeName SPACE 
                       EmployeeGrade SPACE EmployeeSalary
               END-READ
           END-PERFORM
           DISPLAY '------------------------------------------'.
           CLOSE EmployeeFile.
       
       UpdateEmployee.
           OPEN I-O EmployeeFile
           IF WS-File-Status NOT = '00'
               DISPLAY 'Error opening EmployeeFile for I-O'
               STOP RUN
           END-IF

           DISPLAY '------------------------------------------'.
           DISPLAY 'Enter Employee ID to update:' WITH NO ADVANCING.
           ACCEPT WS-EmployeeID.

           PERFORM UNTIL WS-File-Status = '10'
               READ EmployeeFile INTO EmployeeRecord
               AT END
                   EXIT PERFORM
               NOT AT END
                   IF EmployeeID = WS-EmployeeID
                       DISPLAY 'Enter new Employee Name: ' 
                       WITH NO ADVANCING
                       ACCEPT WS-EmployeeName
                       DISPLAY 'Enter new Employee Grade: ' 
                       WITH NO ADVANCING
                       ACCEPT WS-EmployeeGrade
                       DISPLAY 'Enter new Employee Salary: ' 
                       WITH NO ADVANCING
                       ACCEPT WS-EmployeeSalary

                       MOVE WS-EmployeeName TO EmployeeName
                       MOVE WS-EmployeeGrade TO EmployeeGrade
                       MOVE WS-EmployeeSalary TO EmployeeSalary
                       REWRITE EmployeeRecord

                       IF WS-File-Status NOT = '00'
                           DISPLAY 'Error updating EmployeeFile'
                       END-IF
                       EXIT PERFORM
                   END-IF
               END-READ
           END-PERFORM
           DISPLAY 'Employee updated successfully'.
           DISPLAY '------------------------------------------'.
           CLOSE EmployeeFile.
       
       DeleteEmployee.
           OPEN I-O EmployeeFile
           IF WS-File-Status NOT = '00'
               DISPLAY 'Error opening EmployeeFile for I-O'
               STOP RUN
           END-IF
           OPEN OUTPUT TempFile
           IF WS-File-Status NOT = '00'
               DISPLAY 'Error opening TempFile for output'
               STOP RUN
           END-IF

           DISPLAY '------------------------------------------'.
           DISPLAY 'Enter Employee ID to delete:' WITH NO ADVANCING.
           ACCEPT WS-EmployeeID.

           PERFORM UNTIL WS-File-Status = '10'
               READ EmployeeFile INTO EmployeeRecord
               AT END
                   EXIT PERFORM
               NOT AT END
                   IF EmployeeID NOT = WS-EmployeeID
                       WRITE TempRecord FROM EmployeeRecord
                   END-IF
               END-READ
           END-PERFORM
           DISPLAY 'Employee deleted successfully'.
           DISPLAY '------------------------------------------'.
           CLOSE EmployeeFile
           CLOSE TempFile
           CALL 'SYSTEM' USING 'DEL employees.dat'
           CALL 'SYSTEM' USING 'REN temp.dat employees.dat'.

