;******************* common utilities ***********************

;constants
;*************************************************************
(define nil '())

;functions
;************************************************************
(define (find obj list)
  (memq obj list))

;replace item1 with item2 in given list
(define (replace list item1 item2 . eq-pred?)
  (let ((eq-pred? (if (null? eq-pred?) eq? (first eq-pred?))))
    (map (lambda (x)
           (if (list? x)
               (replace x item1 item2 eq-pred?)
               (if (eq? x item1) item2 x))) list)))


;macros
;*************************************************************
;when macro
(define-syntax when
  (syntax-rules () 
    ((when test stmt1 stmt2 ...)
     (if test
         (begin stmt1 stmt2 ...)))))

;unless macro
(define-syntax unless
  (syntax-rules () 
    ((unless test stmt1 stmt2 ...)
     (if (not test)
         (begin stmt1 stmt2 ...)))))

;macro for (/= num1 num2) that is (not (= number1 number2))
(define-syntax /=
  (syntax-rules ()
    ((/= num1 num2)
     (not (= num1 num2)))))

;macro to return first value of the multiple-values-stmt passed.
;where multiple-values-stmt is a form that on evaluation results
;in (values val1 val2 ...)
(define-syntax first-value
  (syntax-rules ()
    ((first-value multiple-values-stmt)
     (call-with-values
         (lambda () multiple-values-stmt)
       (lambda (fval . othrs) fval)))))

; a macro to mimic with-slots in lisp for records
; syntax:
; (with-record-fields (field1 field2 ...) record-name record
;                     stmt1 stmt2 ...)
; any occurance of field is replaced by (record-name:field record)
; any occurance of set-field! is replaced by set-record-name:field
; CAUTION: this *should* only be used as a top level form and not inside
; any scheme form
(define-syntax with-record-fields
  (syntax-rules ()
    ((with-record-fields (f1 ...) record-name record stmt1 ...)
     (let ((body-as-data (list 'begin 'stmt1 ... ))
           (record-name-str (symbol->string 'record-name)))
       (set! body-as-data
             (replace body-as-data 
                      'f1
                      (list (string->symbol (string-append record-name-str ":" (symbol->string 'f1))) 'record)))
       ...
       (set! body-as-data
             (replace body-as-data
                      (string->symbol (string-append "set-" (symbol->string 'f1) "!"))
                      (string->symbol (string-append "set-" record-name-str ":" (symbol->string 'f1)))))
       ...
       (eval body-as-data (interaction-environment))))))

;************************************************************
(format #t "Utility File Loaded..~%")
