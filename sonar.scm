#!/usr/bin/guile -s
!#

;;(define-module Sonar)
(use-modules (ice-9 textual-ports))

;; Reads a line from the sonar data and returns
;; the result as a number. If the end of the sonar data
;; is reached, then the eof-object is returned
(define (get-sonar-reading port)
  (let ((line (get-line port)))
    (when (not (eof-object? line))
      (set! line (string->number line)))
    line))

;; Returns the next rolling window. This is done by removing
;; the first element of the given window, getting a reading from
;; the sonar port, and appending it to the window. The result is
;; the next rolling window.
;; Example:
;;   window = (1 2 3)
;;   (get-next-window sonar window)
;;   => (2 3 4)
(define (get-next-window port window)
  (append (cdr window) (list (get-sonar-reading port))))

;; Returns the last element in a list as a value, not a pair
(define (get-last-el lst)
  (car (last-pair lst)))

;; Sums all elements in a list recursively
(define (sum-list sum lst)
  (if (> (length lst) 0)
      ;; If there are items in the list, add the first element
      ;; to sum and pass call sum-list on the remainder of the list.
      (sum-list (+ sum (car lst)) (cdr lst))
    ;; If the length of list is 0, then this is the sum
    sum))
    

;; Part 1
;; Implement the first challenge to check if each
;; reading of the sonar is greater or less than the previous reading.
;; This is my initial implementation, and is only here for reference.
(define (find-number-of-increases)
  (call-with-input-file "sonar_readings"
    (lambda (sonar)
      ;; Track number of increases
      (let ((increases 0))
        ;; Define the function to increase the counter if the next
        ;; reading is higher
        (define (compare-readings first second)
  	;; When second is not eof
  	(when (not (eof-object? second))
  	  ;; Compare first and second and increment count if necessary
  	  (when (> second first)
  	    (set! increases (+ increases 1)))
  	  ;; Then compare with the next line
  	  (compare-readings second (get-sonar-reading sonar))))
        ;; Execute the program on the first 2 readings. compare-readings
        ;; will handle consuming the following lines to the end of the file.
        (compare-readings
         (get-sonar-reading sonar)
         (get-sonar-reading sonar))
        (format #t "Increase count is ~a" increases)))))

;; Part 2, we need a 3 measurement sliding window.
(define (scan-three-measurement-windows)
  ;; Begin by reading the first 3 lines, these will be stored in a list
  ;; where we will continuously drop the first element and read the next
  ;; one in order to move our window. This is the same way it is done above.
  (call-with-input-file "sonar_readings"
    (lambda (sonar)
      ;; Set window to a list of the first 3 sonar readings
      ;; Initialize increase-count to 0
      (let ((window (list
		     (get-sonar-reading sonar)
		     (get-sonar-reading sonar)
		     (get-sonar-reading sonar)))
	    (increase-count 0))
	;; Define the recursive function to compare the readings
	(define (compute-increases window)
	  ;; Construct the next sonar window
	  (let ((following-window (get-next-window sonar window)))
	    ;; Make sure the last reading is not EOF
	    (when (not (eof-object? (get-last-el following-window)))
    	      ;; Compare the windows
    	      (when (> (sum-list 0 following-window) (sum-list 0 window))
    	        (set! increase-count (+ increase-count 1)))
    	      ;; Proceed to the next window
    	      (compute-increases following-window))))
	(compute-increases window)
	(format #t "Rolling window increase count is ~a" increase-count)))))

;;(scan-three-measurement-windows)
