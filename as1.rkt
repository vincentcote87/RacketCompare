#lang racket
;CPSC 3740 Assignment #1 programming exercise by Vincent Cote

;Question 1)
;x and y perameters expect to get two lists, lists can be empty lists (),
;lists within lists eg. (()), and/or lists with atoms eg. (1 2)
;The function returns true #t if the two lists are identical else it
;returns false #f

(define (myequal? x y)
  ;Base case, if it reaches here with both lists null than
  ;the lists are equal.
  (if (and (null? x)(null? y))
  #t
  ;Checks if one of the list is null, since they are both not null
  ;then having one of them null means they are not equal, returns false.
  (if (or (null? x)(null? y))
      #f
      ;Check to see if the first element of x and y are both pairs
      ;if so than it calls itself with these two pairs as well
      ;as checking the remaining items in the lists.
      (if (and (pair? (car x))(pair? (car y)))
          (and (myequal? (car x)(car y))(myequal? (cdr x)(cdr y)))
          ;Since they are both not pairs it checks if one of them is
          ;if so then return false as the lists are not equal
          (if (or (pair? (car x))(pair? (car y)))
              #f
              ;Reaching this point means the first values of each
              ;lists are atoms, compare them using equal?
              (if (equal? (car x)(car y))
                  (myequal? (cdr x)(cdr y))
                  #f))))))

;Question 2)
;x is an object and y is a list, the function looks to see if object x
;is part of the top level elements of list y
(define (mymember? x y)
  ;Base case, if the end of the list is reached without
  ;finding a match then it returns false
  (if (null? y)
      #f
      ;Check if x is a pair, if so then x is only compared if
      ;the first element of y is also a pair
      (if (and (pair? x)(pair? (car y)))
          (if (myequal? x (car y))
              #t
              (mymember? x (cdr y)))
          ;If x is a pair but the first element of y is not
          ;or the first element of y is a pair but x is not
          ;then it calls itself again with the remaining elements
          ;of y
          (if (or (and (pair? x)(not (pair? (car y))))(and (not(pair? x))(pair? (car y))))
              (mymember? x (cdr y))
              ;If reached here than both x and the first element of
              ;y are not pairs, compare these atoms and return true
              ;if there is a match, otherwise call itself with
              ;the remaining elements of y
              (if (equal? x (car y))
                  #t
                  (mymember? x (cdr y)))))))
      