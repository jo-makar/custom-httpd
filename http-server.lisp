; Http server implementation
; Loosely based on ch 13 of "Land of Lisp" by Conrad Barski


; Serve http sessions via custom handler
; The handlers write responses to the standard-out stream

(defun serve-http (handler port)
  (labels ((decode-url-param (s)
             (labels ((eater (l)
                       (when l
                         (case (car l)

                           ; A % char should be followed by two hexadecimal digits
                           ; Unclear what should be done if not, pass through unchanged for now
                           (#\%       (if (and (>= (length l) 3) (digit-char-p (cadr l) 16) (digit-char-p (caddr l) 16))
                                        (cons (code-char (parse-integer (coerce (list (cadr l) (caddr l)) 'string) :radix 16))
                                              (eater (cdddr l)))
                                        (cons (car l) (eater (cdr l)))))

                           (#\+       (cons #\space (eater (cdr l))))
                           (otherwise (cons (car l) (eater (cdr l))))))))

               (coerce (eater (coerce s 'list)) 'string)))


           (parse-url-params (s)
             (labels ((tokenizer (s)
                        (if (> (length s) 0)
                          (let ((i (position #\& s)))
                            (cond
                              ((not i)  (list s))
                              ((eq i 0) (tokenizer (subseq s 1)))
                              (t        (cons (subseq s 0 i) (tokenizer (subseq s (1+ i)))))))))

                      (splitter (s)
                        (let ((i (position #\= s)))
                          (if i
                            (cons (decode-url-param (subseq s 0 i)) (decode-url-param (subseq s (1+ i))))
                            (cons (decode-url-param s) nil)))))
    
               (mapcar #'splitter (tokenizer s))))
  

           (parse-request-line (s)
             (let* ((i (position #\space s))
                    (j (position #\space s :start (1+ i))))

               (or (and i j (< i j)) (error "invalid request line"))

               (let ((verb  (subseq s 0 i))
                     (url   (subseq s (1+ i) j))
                     (proto (subseq s (1+ j))))

                 ; TODO Should validate verb and proto

                 (let ((q (position #\? url)))
                   (if q
                     (list (subseq url 0 q) (parse-url-params (subseq url (1+ q))) verb proto)
                     (list url nil verb proto))))))


           (parse-headers (stream)
             (let ((line (read-line stream)))
               (unless (equal line "")
                 (let ((i (position #\: line)))
                   (unless i (error "invalid header line"))

                   (let ((key (string-trim '(#\space #\tab) (subseq line 0 i)))
                         (val (string-trim '(#\space #\tab) (subseq line (1+ i)))))
                     (or (and (> (length key) 0) (> (length val) 0)) (error "invalid header line"))

                     (cons (cons key val) (parse-headers stream))))))))


    (let ((socket (socket-server port)))
      (unwind-protect
        (loop (with-open-stream (stream (socket-accept socket))
                (let* ((request (parse-request-line (read-line stream)))
                       (headers (parse-headers stream))
  
                       (body    (let ((h (assoc "Content-Length" headers :test #'equal)))
                                  (when h
                                    (let ((b (make-string (parse-integer (cdr h)))))
                                      (read-sequence b stream)))))

                       (*standard-output* stream))

                  ; TODO Should use threads for each session (with some fixed upper limit)
                  (funcall handler request headers body))))
          (socket-server-close socket)))))


; Example handler

(defun hello-world (request headers body)
  (let ((stream (make-string-output-stream)))
    (princ "<html><body>Hello world</body></html>" stream)

    (let ((body (get-output-stream-string stream)))
      (format t "HTTP/1.1 200 OK~C~C" #\return #\linefeed)

      (let ((days   '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
            (months '("x" "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")))
        (multiple-value-bind
          (sec min hour date month year day-of-week)
          (decode-universal-time (get-universal-time) 0)

          (format t "Date: ~a, ~2,'0d ~a ~d ~2,'0d:~2,'0d:~2,'0d GMT~C~C"
            (nth day-of-week days)
            date
            (nth month months)
            year
            hour
            min
            sec
            #\return
            #\linefeed)))
      
      (format t "Content-Type: text/html~C~C" #\return #\linefeed)
      (format t "Content-Length: ~d~C~C" (length body) #\return #\linefeed)
      (format t "Connection: close~C~C" #\return #\linefeed)
      (format t "~C~C" #\return #\linefeed)

      (princ body))))

