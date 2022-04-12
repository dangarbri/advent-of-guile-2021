#!/usr/bin/guile -s
!#

;;(define-module Sonar)
(use-modules (ice-9 textual-ports))

(define (get-sonar-reading port)
  (let ((line (get-line port)))
    (when (not (eof-object? line))
      (set! line (string->number line)))
    line))

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
      (format #t "Increase count is ~a" increases))))
      
      
