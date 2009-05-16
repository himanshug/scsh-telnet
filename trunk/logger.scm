;************** just another logging framework ************** 


;External Interface
;*************************************************************

;functions

; Returns a procedure: (lambda (level format-str . args) ...)
; (define (get-logger name) ...)

; (define (set-log-level! level) ... )
; (define (set-log-output-stream! out-stream) ... )

;constants: log-levels
(define +debug+ 0)
(define +info+ 1)
(define +warn+ 2)
(define +error+ 3)
(define +fatal+ 4)

;Implementation
;************************************************************

(define +LOG-OUTPUT-STREAM+ #t)
(define +LOG-LEVEL+ +error+)

; functions
(define (set-log-level! level)
  (set! +LOG-LEVEL+ level))
(define (set-log-output-stream! out-stream)
  (set! +LOG-OUTPUT-STREAM+ out-stream))

(define (get-logger name)
  (lambda (level format-str . args)
    (when (>= level +LOG-LEVEL+)
          (apply write-log (cons name (cons format-str args))))))

(define (write-log name format-str . args)
  (apply format (cons +LOG-OUTPUT-STREAM+ (cons format-str args))))

;Finished
;**************************************************************

(format #t "Logger loaded..~%")
