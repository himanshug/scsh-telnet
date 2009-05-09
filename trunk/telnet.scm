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

;utilities
(define nil '())
(define (find obj list)
  (memq obj list))

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

;----------------------------------------------------------------

;telnet implementation
(define-record telnet
  host
  port
  sock-stream
  (cookedq nil)
  (eof #f)
  (iacseq nil)
  (sb 0)
  (sbdataq nil)
  (option-callback default-option-callback%)
  (sb-option-callback default-sb-option-callback%)
  (char-callback (lambda (c s) (display c)))
  (remove-return-char #t)
  (debug-on #f))

;given a host and (optional)port, initiates a connection
;and returns the telnet record
(define (open-telnet-session host . port)
  (let* ((port (if (null? port) +TELNET-PORT+ (first port)))
         (sock (socket-connect protocol-family/internet socket-type/stream host port)))
    (make-telnet host port sock)))

(define (close-telnet-session tn)
  (close-socket (telnet:sock-stream tn))
  (if (telnet:debug-on tn)
      (format #t "~%Telnet Stream Closed~%")))

(define (default-option-callback% out-stream cmd code)
  (if (and (char=? cmd +DO+) (char=? code +TTYPE+))
      (begin
        (write-string
         (format #f "~a~a~a" +IAC+ +WILL+ +TTYPE+)
         out-stream)
        (write-string (format #f "send back: WILL!~%"))
        nil)
      (let (cc (ok #f))
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
              (write-string (format #f "~a~a~a" +IAC+ cc code)
                            out-stream)
              (write-string (format #f "Send back!~%")))
            (write-string (format #f "IAC ~a not recognized" (char->ascii cmd)))))))


(define (send-sub-terminal-type-is% s-out . ttype)
  (let ((ttype (if (null? ttype) "UNKNOWN" ttype)))
    (write-string
     (format #f "~a~a~a~a~a~a~a" +IAC+ +SB+ +TTYPE+ (ascii->char 0) ttype +IAC+ +SE+)
     s-out)
    (write-string
     (format t "~%(~a)(~a)(~a)(~a)~a(~a)(~a)"
          (char->ascii +IAC+) (char->ascii +SB+) (char->ascii +TTYPE+) 0 ttype (char->ascii +IAC+) (char->ascii +SE+))
     s-out)))

(define (default-sb-option-callback% out-stream sbdata)
  (if (and (char=? (list-ref sbdata 0) +TTYPE+)
           (char=? (list-ref sbdata 1) (ascii->char 1)))
      (send-sub-terminal-type-is% out-stream)))
                


; reads one char from the input port, if data
; is not available then returns nil
(define (read-char-no-hang in)
  (if (char-ready? in)
      (read-char in)
      nil))

(define (process-sock-stream% tn . block-read)
  (let ((sock-stream-in (socket:inport (telnet:socket-stream tn)))
        (sock-stream-out (socket:outport (telnet:socket-stream tn)))
        (char-callback (telnet:char-callback tn))
        (option-callback (telnet:option-callback tn))
        (sb-option-callback (telnet:sb-option-callback tn))
        (debug-on (telnet:debug-on tn))
        (remove-return-char (telnet:remove-return-char tn))
        (block-read (if (null? block-read) #f #t)) 
        c cmd opt (len (length cookedq)))
    (unless eof
            (if block-read
                (set! c (read-char sock-stream-in))
                (set! c (read-char-no-hang sock-stream-in)))
            (if (eof-object? c)
                (set-telnet:eof tn #t)))

    (if (or (telnte:eof tn) (null? c))
        (if (= 0 len)
            ;todo: could not signal telnet-eof, impact?
            +no-data+ +old-data)
        (let loop ()
          ;body here
          (case (length (telnet:iacseq tn))
            ((0) ;;length of iacseq
             (cond 
              ((char=? c +theNULL+))
              ((char=? c (ascii->char 21)))
              ((char=? c +IAC+)
               (append (telnet:iacseq tn) (list c)))
              (else
               (if (= (telnet:sb tn) 0)
                   (unless (and remove-return-char (char=? c #\newline))
                           (when char-callback
                               (char-callback c sock-stream-out)) ;TODO
                           (append (telnet:cookedq tn) (list c)))
                   (append (telnet:sbdataq tn) (list c))))))
            ((1) ;;length of iacseq
             (if (find c (list +DO+ +DONT+ +WILL+ +WONT+))
                 (append (telnet:iacseq tn) (list c))
                 (begin ;;else
                   (set-telnet:iacseq tn nil)
                   (cond
                    ((char=? c +IAC+) ;;+IAC+ +IAC+
                     (if (= 0 (telnet:sb tn))
                         (append (telnet:cookedq tn) (list c))
                         (append (telnet:sbdataq tn) (list c))))
                    ((char=? c +SB+) ;;+IAC+ +SB+
                     (when debug-on (format #t "~%.......SB......~%"))
                     (set-telnet:sb tn 1)
                     (set-telnet:sbdataq tn nil))
                    ((char=? c +SE+) ;;+IAC+ +SE+
                     (set-telnet:sb tn 0)
                     (if (and debug-on
                              (/= 0 (length (telnet:sbdataq tn))))
                         (write-line (format #f "sbdata: ~a" (telnet:sbdataq tn))))
                     (if sb-option-callback ;;TODO: change here
                         (sb-option-callback sock-stream-in))
                     (if debug-on (format #t "~%....SE........~%")))
                    (else
                     (if option-callback ;;TODO: change here
                         (option-callback sock-stream c +NOOPT+)
                         (if debug-on
                             (format #t "IAC ~d not recognized" (char->ascii c)))))))))
            ((2) ;;length of iacseq
             (set! cmd (list-ref (telnet:iacseq tn) 1))
             (set-telnet:iacseq tn nil)
             (set! opt c)
             (cond
              ((or (char=? cmd +DO+)
                   (char=? cmd +DONT+))
               (if debug-on
                   (format #t "IAC ~s ~d~%"
                           (if (char=? cmd +DO+)
                               "DO" "DONT")
                           (char->ascii opt)))
               (if option-callback ;;todo: change here
                   (option-callback sock-stream-in cmd opt)
                   (begin ;;todo: an issue here
                     (write-string (format #f "~a~a~a" +IAC+ +WONT+ opt)))))
              ((or (char=? cmd +WILL+)
                   (char=? cmd +WONT+))
               (if debug-on
                   (format #t "IAC ~s ~d~%" (if (char=? cmd +WILL+) "WILL" "WONT")
                           (char->ascii opt)))
               (if option-callback ;;todo:change here
                   (option-callback sock-stream-in cmd opt)
                   (write-string (format #f "~a~a~a" +IAC+ +DONT+ opt)))))))
          (set! c (read-char-no-hang sock-stream-in))
          (if (eof-object? c)
              (set-telnet:eof tn #t)
              (if (null? c)
                  (if (/= len (length (telnet:cookedq tn)))
                      +new-data+
                      (if (= 0 len)
                          (begin
                            (if (telnet:eof tn)) ;;todo: signal telnet-eof)
                            +no-data+)
                          +old-data))
                  (loop)))))))

(define peek-available-data (tn . block-read)
   (let ((block-read (if (null? block-read) #f #t)))
    (process-sock-stream% tn block-read)
    (apply list (telnet:cookedq tn))))

(define read-available-data (tn . block-read)
  (let ((block-read (if (null? block-read) #f #t)))
    (process-sock-stream% tn block-read)
    (apply list (telnet:cookedq tn))
    (set-telnet:cookedq nil)))

; searches for occurance of string str1 in str2
; returns index of first occurance of str1 or returns -1
(define (search str1 str2 search-start . case-sensitive)
  (let ((result
         (if case-sensitive
             (string-contains str2 str1 search-start)
             (string-contains-ci str2 str1 search-start))))
    (or result -1)))
        
"Read the cookedq from 0 to end-pos."
(define (read-cookedq% tn end-pos)
  (let ((cookedq (telnet:cookedq tn)))
    (if (>= (length cookedq) end-pos)
        (set-telnet:cookedq tn (list-tail cookedq end-pos))
        (set-telnet:cookedq tn nil))))
 

 
;we exit only in three cases
; - we got the string
; - time out
; - reached eof
;;read-until without timeout
(define (read-until tn str case-sensitive)
  (let* ((debug-on (telnet:debug-on tn))
         (sock-stream-in (socket:inport (telnet:sock-stream tn)))
         pos
         (read-status (process-sock-stream% tn))
         data-len
         (search-start 0)
         (str-len (string-length str)))
    (let outer-loop ()
      (set! data-len (length (telnet:cookedq tn)))
      (if (>= (- data-len search-start) str-len)
          (begin 
            (set! pos (search str (list->string (telnet:cookedq tn)) search-start case-sensitive))
            (set! search-start (+ 2 (- data-len 1 str-len))))
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



;(define (wait-until-read% -- Not needed for now
;;a read-until with timeout
(define (read_until tn str char-equal-func . timeout)
  (let* ((debug-on (telnet:debug-on tn))
         (sock-stream-in (socket:inport (telnet:sock-stream tn)))
         start-time
         elasped
         (timeout (if (null? timeout) 600 #f))
         (block-read (if timeout #f #t))
         pos
         (read-status (process-sock-stream% tn))
         data-len
         (search-start 0)
         (str-len (string-length str)))
    (if timeout (set! start-time (time)))
    (let outer-loop ()
      (set! data-len (length (telnet:cookedq tn)))
      (if (>= (- data-len search-start) str-len)
          (begin 
            (set! pos (search (string->list str) (telnet:cookedq tn) search-start char-equal-func))
            (set! search-start (+ 2 (- data-len 1 str-len))))
          (set! pos -1))

      (if (>= pos 0)
          (begin
            (set! pos (+ pos str-len))
            (values (read-cookedq% tn pos) +ok+))
          (if (telnet:eof tn)
              (begin
                (display "eof!!")
                (values nil +eof+))
              (let inner-loop ()
                (set! elasped (- (time) start-time))
                (if (>= elasped timeout)
                    (begin
                      (display "Timeout!")
                      (values nil +timeout+))
                    (case (wait-until-readable% sock-stream-in (- timeout elasped) debug-on)
                      ((+eof+)
                       (set-telnet:eof #t)
                       (values nil +eof+))
                      ((+timeout+) (values nil +timeout+))
                      (else
                       ;;todo: (sleep 0.25)
                       (set! read-status (process-sock-stream% tn block-read))
                       (if (= read-status +new-data+)
                           (outer-loop)
                           (if (telnet:eof tn)
                               (begin
                                 (display "eof!")
                                 (values nil +eof+))
                               (inner-loop))))))))))))