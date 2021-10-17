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

            (sb-thread:make-thread
              (lambda ()
                (handler-case

                  (with-open-stream (client-stream (sb-bsd-sockets:socket-make-stream
                                                     client-socket :input t :output t))

                    (loop
                      (let ((request (parse-request-line (header-read-line client-stream)))
                            (headers (parse-headers client-stream)))

                        (funcall handler (list client-stream client-socket) request headers)

                        (or (open-stream-p client-stream) (return)))))
                    
                  (end-of-file (c)
                    (declare (ignore c))
                    (sb-bsd-sockets:socket-close client-socket))

                  (t (c)
                    (sb-bsd-sockets:socket-close client-socket)
                    (format t "~a" c)))))))

        (t (c)
          (sb-bsd-sockets:socket-close server-socket)
          (format t "~a" c))))))


(defun http-server/hello-world-handler (client request headers)
  (declare (ignore request headers))

  (let ((client-stream (car client))
        (body-stream (make-string-output-stream)))
    (princ "<html><body>Hello world</body></html>" body-stream)

    (let ((body (get-output-stream-string body-stream)))
      (format client-stream "HTTP/1.1 200 OK~c~c" #\return #\linefeed)
      (format client-stream "Content-Type: text/html~c~c" #\return #\linefeed)
      (format client-stream "Content-Length: ~d~c~c" (length body) #\return #\linefeed)
      (format client-stream "Connection: close~c~c" #\return #\linefeed)
      (format client-stream "~c~c" #\return #\linefeed)

      (princ body client-stream)

      (close client-stream))))


(defun http-server/seekable-handler (client request headers)
  (declare (ignore headers)) ; FIXME

  (let ((client-stream (car client))
        (client-socket (cadr client))
        (verb          (car request))
        (path          (cadr request)))

    (labels ((response (status &optional body content-type)

               (let ((statuses '((200 . "OK")
                                 (400 . "Bad Request")
                                 (403 . "Forbidden"))))

                 (format client-stream "HTTP/1.1 ~d ~a~c~c"
                   status (cdr (assoc status statuses)) #\return #\linefeed))

               (let ((days   '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
                     (months '("x" "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")))

                 (multiple-value-bind
                   (sec min hour date month year day)
                   (decode-universal-time (get-universal-time) 0)

                   (format client-stream "Date: ~a, ~2,'0d ~a ~d ~2,'0d:~2,'0d:~2,'0d GMT~c~c"
                     (nth day days) date (nth month months) year hour min sec #\return #\linefeed)))

               (format client-stream "Content-Length: ~d~c~c" (length body) #\return #\linefeed)

               (when body
                 (or content-type (setq content-type "text/html"))
                 (format client-stream "Content-Type: ~a~c~c" content-type #\return #\linefeed))

               (let ((ok (and (< status 300) (>= status 200))))
                 (format client-stream "Connection: ~a~c~c"
                   (if ok "keep-alive" "close") #\return #\linefeed)

                 (format client-stream "~c~c" #\return #\linefeed)

                 (when body
                   (write-sequence body client-stream)
                   (finish-output client-stream))

                 (unless ok (close client-stream))

                 (return-from http-server/seekable-handler)))

             (ip-to-string (ip)
               (reduce (lambda (a b)
                         (concatenate 'string a "." b))
                       (mapcar #'write-to-string (coerce ip 'list)))))

      (multiple-value-bind (ip port) (sb-bsd-sockets:socket-peername client-socket)
        (format t "~a:~d: ~a ~a~%" (ip-to-string ip) port verb path))

      (unless (equal (string-upcase verb) "GET") (response 400))

      ; FIXME STOPPED determine if path valid and if for a file or dir and handle appropriately
      ;               index pages should support sorting by filename, size and date (via javascript)
      (response 200 (format nil "<html><body>~a</body></html>" path))
      )))


(http-server/serve #'http-server/seekable-handler)

