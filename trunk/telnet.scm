; ------ telnet protocol constants -------

;; Telnet protocol defaults
(define +TELNET-PORT+ 23)

;; Telnet protocol characters (don't change)
(define +IAC+ (ascii->char 255)) ;;  "Interpret As Command"
(define +DONT+ (ascii->char 254))
(define +DO+ (ascii->char 253))
(define +WONT+ (ascii->char 252))
(define +WILL+ (ascii->char 251))
(define +theNULL+ (ascii->char 0))

(define +SE+ (ascii->char 240))  ;;  Subnegotiation End
(define +NOP+ (ascii->char 241))  ;;  No Operation
(define +DM+ (ascii->char 242))  ;;  Data Mark
(define +BRK+ (ascii->char 243))  ;;  Break
(define +IP+ (ascii->char 244))  ;;  Interrupt process
(define +AO+ (ascii->char 245))  ;;  Abort output
(define +AYT+ (ascii->char 246))  ;;  Are You There
(define +EC+ (ascii->char 247))  ;;  Erase Character
(define +EL+ (ascii->char 248))  ;;  Erase Line
(define +GA+ (ascii->char 249))  ;;  Go Ahead
(define +SB+ (ascii->char 250))  ;;  Subnegotiation Begin


;; Telnet protocol options code (don't change)
;; These ones all come from arpa/telnet.h
(define +BINARY+ (ascii->char 0)) ;;  8-bit data path
(define +ECHO+ (ascii->char 1)) ;;  echo
(define +RCP+ (ascii->char 2)) ;;  prepare to reconnect
(define +SGA+ (ascii->char 3)) ;;  suppress go ahead
(define +NAMS+ (ascii->char 4)) ;;  approximate message size
(define +STATUS+ (ascii->char 5)) ;;  give status
(define +TM+ (ascii->char 6)) ;;  timing mark
(define +RCTE+ (ascii->char 7)) ;;  remote controlled transmission and echo
(define +NAOL+ (ascii->char 8)) ;;  negotiate about output line width
(define +NAOP+ (ascii->char 9)) ;;  negotiate about output page size
(define +NAOCRD+ (ascii->char 10)) ;;  negotiate about CR disposition
(define +NAOHTS+ (ascii->char 11)) ;;  negotiate about horizontal tabstops
(define +NAOHTD+ (ascii->char 12)) ;;  negotiate about horizontal tab disposition
(define +NAOFFD+ (ascii->char 13)) ;;  negotiate about formfeed disposition
(define +NAOVTS+ (ascii->char 14)) ;;  negotiate about vertical tab stops
(define +NAOVTD+ (ascii->char 15)) ;;  negotiate about vertical tab disposition
(define +NAOLFD+ (ascii->char 16)) ;;  negotiate about output LF disposition
(define +XASCII+ (ascii->char 17)) ;;  extended ascii character set
(define +LOGOUT+ (ascii->char 18)) ;;  force logout
(define +BM+ (ascii->char 19)) ;;  byte macro
(define +DET+ (ascii->char 20)) ;;  data entry terminal
(define +SUPDUP+ (ascii->char 21)) ;;  supdup protocol
(define +SUPDUPOUTPUT+ (ascii->char 22)) ;;  supdup output
(define +SNDLOC+ (ascii->char 23)) ;;  send location
(define +TTYPE+ (ascii->char 24)) ;;  terminal type
(define +EOR+ (ascii->char 25)) ;;  end or record
(define +TUID+ (ascii->char 26)) ;;  TACACS user identification
(define +OUTMRK+ (ascii->char 27)) ;;  output marking
(define +TTYLOC+ (ascii->char 28)) ;;  terminal location number
(define +VT3270REGIME+ (ascii->char 29)) ;;  3270 regime
(define +X3PAD+ (ascii->char 30)) ;;  X.3 PAD
(define +NAWS+ (ascii->char 31)) ;;  window size
(define +TSPEED+ (ascii->char 32)) ;;  terminal speed
(define +LFLOW+ (ascii->char 33)) ;;  remote flow control
(define +LINEMODE+ (ascii->char 34)) ;;  Linemode option
(define +XDISPLOC+ (ascii->char 35)) ;;  X Display Location
(define +OLD_ENVIRON+ (ascii->char 36)) ;;  Old - Environment variables
(define +AUTHENTICATION+ (ascii->char 37)) ;;  Authenticate
(define +ENCRYPT+ (ascii->char 38)) ;;  Encryption option
(define +NEW_ENVIRON+ (ascii->char 39)) ;;  New - Environment variables
;; the following ones come from
;; http://www.iana.org/assignments/telnet-options
;; Unfortunately, that document does not assign identifiers
;; to all of them, so we are making them up
(define +TN3270E+ (ascii->char 40)) ;;  TN3270E
(define +XAUTH+ (ascii->char 41)) ;;  XAUTH
(define +CHARSET+ (ascii->char 42)) ;;  CHARSET
(define +RSP+ (ascii->char 43)) ;;  Telnet Remote Serial Port
(define +COM_PORT_OPTION+ (ascii->char 44)) ;;  Com Port Control Option
(define +SUPPRESS_LOCAL_ECHO+ (ascii->char 45)) ;;  Telnet Suppress Local Echo
(define +TLS+ (ascii->char 46)) ;;  Telnet Start TLS
(define +KERMIT+ (ascii->char 47)) ;;  KERMIT
(define +SEND_URL+ (ascii->char 48)) ;;  SEND-URL
(define +FORWARD_X+ (ascii->char 49)) ;;  FORWARD_X
(define +PRAGMA_LOGON+ (ascii->char 138)) ;;  TELOPT PRAGMA LOGON
(define +SSPI_LOGON+ (ascii->char 139)) ;;  TELOPT SSPI LOGON
(define +PRAGMA_HEARTBEAT+ (ascii->char 140)) ;;  TELOPT PRAGMA HEARTBEAT
(define +EXOPL+ (ascii->char 255)) ;;  Extended-Options-List
(define +NOOPT+ (ascii->char 0))

;----------------------------------------------------------------

;other constants
(define +no-data+ 1)
(define +old-data+ 2)
(define +new-data+ 3)
(define +timeout+  4)
(define +eof+ 5)
(define +ok+ 6)
(define +CR+ (ascii->char 13)) ;carriage return character

;utilities
(define nil '())
(define (find obj list)
  (memq obj list))

;replace item1 with item2 in given list
(define (replace list item1 item2 . eq-pred?)
  (let ((eq-pred? (if (null? eq-pred?) eq? eq-pred?)))
    (map (lambda (x)
           (if (list? x)
               (replace x item1 item2 eq-pred?)
               (if (eq? x item1) item2 x))) list)))

;destructive append

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

;----------------------------------------------------------------

;telnet implementation
(define-record telnet
  host
  port
  sock
  (cookedq nil)
  (eof #f)
  (iacseq nil)
  (sb 0)
  (sbdataq nil)
  (option-callback default-option-callback%)
  (sb-option-callback default-sb-option-callback%)
  (char-callback (lambda (c s)
                   (format #t "himanshu: char call back, ~d~%" 
                           (char->ascii c))
                   ;(display c)
                   ))
  (remove-return-char #t)
  (debug-on #f))

;given a host and (optional)port, initiates a connection
;and returns the telnet record
(define (open-telnet-session host . port)
  (let* ((port (if (null? port) +TELNET-PORT+ (first port)))
         (sock (socket-connect protocol-family/internet socket-type/stream host port)))
    (make-telnet host port sock)))

(define (close-telnet-session tn)
  (close-socket (telnet:sock tn))
  (if (telnet:debug-on tn)
      (format #t "~%Telnet Stream Closed~%")))

(define (default-option-callback% out-stream cmd code)
  (format #t "himanshu: default option callback~%")
  (if (and (char=? cmd +DO+) (char=? code +TTYPE+))
      (begin
         (format out-stream "~a~a~a" +IAC+ +WILL+ +TTYPE+)
         (format #t "send back: WILL!~%")
        nil)
      (let ((cc nil) (ok #f))
        (cond ((or (char=? cmd +WILL+)
                   (char=? cmd +WONT+))
               (set! cc +DONT+)
               (write-string "DONT")
               (set! ok #t))
              ((or (char=? cmd +DO+)
                   (char=? cmd +DONT+))
               (set! cc +WONT+)
               (write-string "WONT")
               (set! ok #t)))
        (if ok
            (begin
              (format out-stream "~a~a~a" +IAC+ cc code)
              (format #t "Send back!~%")))
        (format #t "IAC ~a not recognized" (char->ascii cmd)))))


(define (send-sub-terminal-type-is% s-out . ttype)
  (let ((ttype (if (null? ttype) "UNKNOWN" ttype)))
    (format s-out "~a~a~a~a~a~a~a" +IAC+ +SB+ +TTYPE+ (ascii->char 0) ttype +IAC+ +SE+)
    (format #t "~%(~a)(~a)(~a)(~a)~a(~a)(~a)"
            (char->ascii +IAC+) (char->ascii +SB+) (char->ascii +TTYPE+) 0 ttype (char->ascii +IAC+) (char->ascii +SE+))))
(define (default-sb-option-callback% out-stream sbdata)
  (format #t "himanshu:sb-option-callback~%")
  (if (and (char=? (list-ref sbdata 0) +TTYPE+)
           (char=? (list-ref sbdata 1) (ascii->char 1)))
      (send-sub-terminal-type-is% out-stream)))
                


; reads one char from the input port, if data
; is not available then returns nil
(define (read-char-no-hang in)
  (if (char-ready? in)
      (read-char in)
      nil))

; Internal procedure to read all the available data from the
; socket to cookedq. By default its a non-blocking call.
; Return : +no-data+, when no data filled the cookedq.
; Return : +old-data+, when no new data read from the socket but cookedq has
;   old data.
; Return : +new-data+, when new data are read from socket stream to cookedq.

(with-record-fields
 (sock char-callback option-callback sb-option-callback sb
       debug-on remove-return-char cookedq eof sbdataq iacseq)
 telnet tn
 (define (process-sock-stream% tn . block-read)
   (let ((sock-stream-in (socket:inport sock))
         (sock-stream-out (socket:outport sock))
         (block-read (if (null? block-read) #f (first block-read))) 
         (c nil) (cmd nil) (opt nil) (len (length cookedq)))
     (unless eof
             (if block-read
                 (set! c (read-char sock-stream-in))
                 (set! c (read-char-no-hang sock-stream-in)))
             (if (eof-object? c)
                 (set-eof! tn #t)))

     (if (or eof (null? c))
         (if (= 0 len) +no-data+ +old-data+)
         (let loop ()
           (format #t "himanshu: got inside loop, c:~d~%"
                   (char->ascii c))
           (format #t "himanshu: cookedq:~a~%" (telnet:cookedq tn))
           (case (length iacseq)
             ((0) ;;length of iacseq
              (cond
               ((char=? c +theNULL+))
               ((char=? c (ascii->char 21)))
               ((char=? c +IAC+)
                (set-iacseq! ;TODO; use destructive append
                 tn (append iacseq (list c))))
               (else
                (if (= sb 0)
                    (unless (and remove-return-char (char=? c +CR+))
                            (when char-callback
                                  (char-callback c sock-stream-out))
                            (set-cookedq! tn (append cookedq (list c)))) ;TODO; use destructive append
                    (set-telnet:sbdataq tn (append sbdataq (list c))))))) ;TODO; use destructive append
             ((1) ;;length of iacseq
              (if (find c (list +DO+ +DONT+ +WILL+ +WONT+))
                  (set-iacseq! tn (append iacseq (list c))) ;TODO; use destructive append
                  (begin ;;else
                    (set-iacseq! tn nil)
                    (cond
                     ((char=? c +IAC+) ;;+IAC+ +IAC+
                      (if (= 0 sb)
                          (set-cookedq! tn (append cookedq (list c))) ;TODO; use destructive append
                          (set-sbdataq! tn (append sbdataq (list c))))) ;TODO; use destructive append
                     ((char=? c +SB+) ;;+IAC+ +SB+
                      (when debug-on (format #t "~%.......SB......~%"))
                      (set-sb! tn 1)
                      (set-sbdataq! tn nil))
                     ((char=? c +SE+) ;;+IAC+ +SE+
                      (set-sb! tn 0)
                      (if (and debug-on
                               (/= 0 (length sbdataq)))
                          (write-string (format #f "sbdata: ~a" sbdataq)))
                      (if sb-option-callback ;;TODO: change here
                          (sb-option-callback sock-stream-out sbdataq))
                      (if debug-on (format #t "~%....SE........~%")))
                     (else
                      (if option-callback
                          (option-callback sock-stream-out c +NOOPT+)
                          (if debug-on
                              (format #t "IAC ~d not recognized" (char->ascii c)))))))))
             ((2) ;;length of iacseq
              (set! cmd (list-ref iacseq 1))
              (set-iacseq! tn nil)
              (set! opt c)
              (cond
               ((or (char=? cmd +DO+)
                    (char=? cmd +DONT+))
                (if debug-on
                    (format #t "IAC ~s ~d~%"
                            (if (char=? cmd +DO+)
                                "DO" "DONT")
                            (char->ascii opt)))
                (if option-callback
                    (option-callback sock-stream-out cmd opt)
                    (begin
                      (write-string (format #f "~a~a~a" +IAC+ +WONT+ opt) sock-stream-out))))
               ((or (char=? cmd +WILL+)
                    (char=? cmd +WONT+))
                (if debug-on
                    (format #t "IAC ~s ~d~%" (if (char=? cmd +WILL+) "WILL" "WONT")
                            (char->ascii opt)))
                (if option-callback
                    (option-callback sock-stream-out cmd opt)
                    (write-string (format #f "~a~a~a" +IAC+ +DONT+ opt) sock-stream-out))))))
           (set! c (read-char-no-hang sock-stream-in))
           (if (eof-object? c)
               (set-eof! tn #t)
               (if (null? c)
                   (if (/= len (length cookedq))
                       +new-data+
                       (if (= 0 len)
                           (begin
                                        ;(if (telnet:eof tn)) ;;todo: signal telnet-eof)
                             +no-data+)
                           +old-data+))
                   (loop))))))))

(define (peek-available-data tn . block-read)
   (let ((block-read (if (null? block-read) #f (first block-read))))
    (process-sock-stream% tn block-read)
    (apply list (telnet:cookedq tn))))

(define (read-available-data tn . block-read)
  (let ((block-read (if (null? block-read) #f (first block-read)))
        (result nil))
    (process-sock-stream% tn block-read)
    (set! result (apply list (telnet:cookedq tn)))
    (set-telnet:cookedq nil)
    result))

; searches for occurance of string str1 in str2
; Param: search-start - index where to start the search from in str2
; Param: case-sensitive - should do a case sensitive or insensitive
;   match, by default it is true.
; Return: index of first occurance of str1 or returns -1
(define (search str1 str2 search-start . case-sensitive)
  (let* ((case-sensitive (if (null? case-sensitive) #t
                             (first case-sensitive)))
         (result
         (if case-sensitive
             (string-contains str2 str1 search-start)
             (string-contains-ci str2 str1 search-start))))
    (or result -1)))
        
; Read the cookedq from 0 to end-pos(excluding the character AT end-pos
(define (read-cookedq% tn end-pos)
  (let ((cookedq (telnet:cookedq tn)))
    (if (>= (length cookedq) end-pos)
        (set-telnet:cookedq tn (list-tail cookedq end-pos))
        (set-telnet:cookedq tn nil))))
 
; Read until a given string is encountered.
; When no match is found, return nil with a +eof+ or +timeout+, and
; all the received data will still stay in the cookedq.
(define (read-until tn str case-sensitive)
  (let* ((debug-on (telnet:debug-on tn))
         (sock-stream-in (socket:inport (telnet:sock tn)))
         (pos nil)
         (block-read #t)
         (read-status (process-sock-stream% tn))
         (data-len nil)
         (search-start 0)
         (str-len (string-length str)))
    (let outer-loop ()
      (set! data-len (length (telnet:cookedq tn)))
      (if (>= (- data-len search-start) str-len)
          (begin 
            (set! pos (search str (list->string (telnet:cookedq tn)) search-start case-sensitive))
            (set! search-start (+ 2 (- data-len (+ 1 str-len)))))
          (set! pos -1))

      (if (>= pos 0)
          (begin
            (set! pos (+ pos str-len))
            (values (read-cookedq% tn pos) +ok+))
          (if (telnet:eof tn)
              (begin
                (display "eof!!")
                (values nil +eof+))
              (begin
                (let inner-loop ()
                  ;;sleep for some time
                  (set! read-status (process-sock-stream% tn block-read))
                  (if (= read-status +new-data+)
                      (outer-loop)
                      (if (telnet:eof tn)
                          (begin
                            (display "eof!")
                            (values nil +eof+))
                          (inner-loop))))))))))


(define (write-ln tn str)
  (format (socket:outport (telnet:sock tn)) "~a~%" str))