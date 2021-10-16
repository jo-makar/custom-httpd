(require 'sb-bsd-sockets)


(define-condition http-server/invalid-request-line
                  (error)
                  ((line :initarg :line
                         :reader line))
  (:report (lambda (c s)
             (format s "invalid request line: ~S" (line c)))))


(define-condition http-server/invalid-header
                  (error)
                  ((line :initarg :line
                           :reader line))
  (:report (lambda (c s)
             (format s "invalid header: ~S" (line c)))))


(defun http-server/serve (handler &optional port addr)
  (or port (setq port 8000))
  (or addr (setq addr #(0 0 0 0)))

  (labels ((header-read-line (stream)
             (string-right-trim '(#\return) (read-line stream)))


           (parse-request-line (line)
             (let* ((i (position #\space line))
                    (j (and i (position #\space line :start (1+ i)))))

               (or (and i j (> i 0) (> j (1+ i)) (< j (1- (length line))))
                   (error (make-condition 'http-server/invalid-request-line :line line)))

               (let ((verb  (subseq line 0 i))
                     (path  (subseq line (1+ i) j))
                     (proto (subseq line (1+ j))))

                 (or (every #'alpha-char-p verb)
                     (error (make-condition 'http-server/invalid-request-line :line line)))

                 (let ((p (string-upcase proto)))
                   (or (equal p "HTTP/1.1")
                       (equal p "HTTP/1.0")
                       (error (make-condition 'http-server/invalid-request-line :line line))))

                 (let ((q (position #\? path)))
                   (if q
                     (list verb (subseq path 0 q) (subseq path q) proto)
                     (let ((f (position #\# path)))
                       (if f
                         (list verb (subseq path 0 f) (subseq path f) proto)
                         (list verb path nil proto))))))))

              
           (parse-headers (stream)
             (let ((lines (labels ((eater (stream consumed)
                                     (let ((line (header-read-line stream)))
                                       (if (equal line "")
                                         (reverse consumed)
                                         (eater stream (cons line consumed))))))
                            (eater stream '()))))

               (labels ((parser (line)
                          (let ((i (position #\: line)))

                            (or (and i (> i 0) (< i (1- (length line))))
                                (error (make-condition 'http-server/invalid-header :line line)))

                            (let ((key (subseq line 0 i))
                                  (val (string-trim '(#\space #\tab) (subseq line (1+ i)))))

                              (or (> (length val) 0)
                                  (error (make-condition 'http-server/invalid-header :line line)))

                              (cons key val)))))

                 (mapcar #'parser lines)))))


    (let ((server-socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
      (setf (sb-bsd-sockets:sockopt-reuse-address server-socket) t)
      (sb-bsd-sockets:socket-bind server-socket addr port)
      (sb-bsd-sockets:socket-listen server-socket 1)

      (handler-case
        (loop
          (let ((client-socket (sb-bsd-sockets:socket-accept server-socket)))
            (handler-case

              (sb-thread:make-thread
                (lambda ()
                  (with-open-stream (client-stream (sb-bsd-sockets:socket-make-stream
                                                     client-socket :input t :output t))
                    (loop
                      (let ((request (parse-request-line (header-read-line client-stream)))
                            (headers (parse-headers client-stream)))

                        (funcall handler client-stream request headers)

                        (or (open-stream-p client-stream) (return)))))))

              (end-of-file (c)
                (declare (ignore c))
                (sb-bsd-sockets:socket-close client-socket))
              (t (c)
                (sb-bsd-sockets:socket-close client-socket)
                (format t "~a" c)))))

          (t (c)
            (sb-bsd-sockets:socket-close server-socket)
            (format t "~a" c))))))


(defun http-server/hello-world-handler (client request headers)
  (declare (ignore request headers))

  (let ((stream (make-string-output-stream)))
    (princ "<html><body>Hello world</body></html>" stream)

    (let ((body (get-output-stream-string stream)))
      (format client "HTTP/1.1 200 OK~c~c" #\return #\linefeed)
      (format client "Content-Type: text/html~c~c" #\return #\linefeed)
      (format client "Content-Length: ~d~c~c" (length body) #\return #\linefeed)
      (format client "Connection: close~c~c" #\return #\linefeed)
      (format client "~c~c" #\return #\linefeed)

      (princ body client)

      (close client))))


; FIXME
(http-server/serve #'http-server/hello-world-handler)

; FIXME index pages should support sorting by filename, size and date
; FIXME the seekable handler should set Connection: keep-alive and not close the socket stream

;(defun http-server/date-header ()
;  (let ((days   '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
;        (months '("x" "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")))
;    (multiple-value-bind
;      (sec min hour date month year day-of-week)
;      (decode-universal-time (get-universal-time) 0)
;
;      (format nil "Date: ~a, ~2,'0d ~a ~d ~2,'0d:~2,'0d:~2,'0d GMT~c~c"
;        (nth day-of-week days)
;        date
;        (nth month months)
;        year
;        hour
;        min
;        sec
;        #\return
;        #\linefeed))))
