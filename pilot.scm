;; This file manages controls for the submarine!
;; The submarine is technically on autopilot, but we
;; need to figure out where it's going. We can read the
;; submarines autopilot path and figure out where we're going.

;; Define the current submarine positions. Starting at position 0.
(define horizontal-position 0)
(define depth 0)
;; Part 2, we also must track an "aim"
(define aim 0)

;; Define the submarine control commands. The submarine does not
;; move in reverse. These are for part 1.
;;(define (forward amount)
;;  (set! horizontal-position (+ horizontal-position amount)))
;;
;;(define (down amount)
;;  (set! depth (+ depth amount)))
;;
;;(define (up amount)
;;  (set! depth (- depth amount)))

;; Part 2 movement commands
;; I think my implementation is pretty neat. Part 2 changes how up/down
;; and forward work. So I was able to keep the same autopilot code and
;; simply redefine these.
(define (down amt)
  (set! aim (+ aim amt)))

(define (up amt)
  (set! aim (- aim amt)))

(define (forward amt)
  (set! horizontal-position (+ horizontal-position amt))
  (set! depth (+ depth (* aim amt))))

;; Functions for parsing the autopilot commands
(use-modules (ice-9 textual-ports)) ;; for get-line

;; Schemify command - This is clever magic to convert a string
;; like "forward 1" to the scheme command (forward 1)
;; This will be used to execute the autopilot file. Bad security,
;; good magic.
(define (schemify-command command-string)
  (let ((strlst (string-split command-string #\ )))
    (list-set! strlst 0 (string->symbol (car strlst)))
    (list-set! strlst 1 (string->number (car (cdr strlst))))
    strlst))

;; Returns the next command from the given port as a scheme
;; expression. If reading the last line, the empty list is returned.
(define (get-command port)
  (let ((command (get-line port)))
    (if (eof-object? command)
	'() ;; If command is eof, return empty list
      (schemify-command command))))
  

(define (compute-destination autopilot_file)
  (call-with-input-file autopilot_file
    (lambda (pilot)
      (define (execute-autopilot)
	(let ((cmd (get-command pilot)))
	  (when (not (= 0 (length cmd))) ;; When the command is not empty
	    (primitive-eval cmd)         ;; Execute the command
	    (execute-autopilot))))       ;; And execute the next command
      (execute-autopilot) ;; Execute all autopilot commands
      (format #t "Horizontal Position: ~a; Depth: ~a" horizontal-position depth))))

(compute-destination "autopilot_path")
(newline)
(format #t "Result: ~a" (* horizontal-position depth))
(newline)
