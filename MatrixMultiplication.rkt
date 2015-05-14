; Imports
#lang racket

; Function: newIdentityMatrix
; Purpose: Creates an identity matrix with size n x n, where n is specified by the calling function
(define (newIdentityMatrix n)
  (let iloop ((i n) (matrix '()))
    (if (= i 0) matrix  ; If base case reached, return our matrix
        (iloop (- i 1)  ; Else, reiterate
               (cons (let jloop ((j n) (row '()))
                       (if (= j 0) row  ; If base case reached, return our new row
                           (jloop (- j 1)  ; Else, reiterate
                                  (cons (if (= i j) 1 0) row)  ; If we're on the diagonal, set the entry's value to 1
                           )  ; End else
                       )
                     ) matrix
               )
        )  ; End else
    )
  )
)

; Function: transposeMatrix
; Purpose: Takes in a matrix and swaps its rows with its columns
(define (transposeMatrix M)
  (if (null? (car M)) '()  ; If base case reached, we're done with this row, append an empty list
      (cons (map car M) (transposeMatrix (map cdr M)))  ; Extract the first entry of each row, and append the next appropriate entry
  )
)

; Function: addMatrices
; Purpose: Takes in two matrices and finds the sum of matching entries
(define (addMatrices M1 M2)
  (map (lambda (row1 row2) (map + row1 row2)) M1 M2))  ; For each row of M1 and M2, apply addition across each entry

; Define whatever matrices we want here
(define M1 '((1 2 3) (2 3 4) (3 2 1)))
(define M2 (newIdentityMatrix 3))

; Demonstrate our matrix operations
(write 'Matrix1:)
(newline)
(write M1)
(newline)
(write 'Matrix2:)
(newline)
(write M2)
(newline)
(newline)
(write 'Matrix1Transpose:)
(newline)
(write (transposeMatrix M1))
(newline)
(write 'Matrix2Transpose:)
(newline)
(write (transposeMatrix M2))
(newline)
(write 'MatrixSum:)
(newline)
(write (addMatrices M1 M2))
(newline)