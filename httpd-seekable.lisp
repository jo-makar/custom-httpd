(require 'sb-bsd-sockets)
(require 'sb-posix)


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

                 (let ((q (position #\? path))
                       (f (position #\# path)))
                   
                   (cond
                     ((and (null q) (null f))
                       (list verb path nil nil proto))
                     
                     ((and (null q) f)
                       (list verb (subseq path 0 f) nil (subseq path f) proto))
                     
                     ((or (and q (null f)) (and q f (< f q)))
                       (list verb (subseq path 0 q) (subseq path q) nil proto))
                     
                     (t ; (and q f (< q f))
                       (list verb (subseq path 0 q) (subseq path q f) (subseq path f) proto)))))))

              
           (parse-headers (stream)
             (let ((lines (labels ((eater (stream consumed)
                                     (let ((line (header-read-line stream)))
                                       (if (equal line "")
                                         (reverse consumed)
                                         (eater stream (cons line consumed))))))
                            (eater stream '()))))

               (flet ((parser (line)
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
                    (format t "~a~%" c)))))))

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
  (let* ((client-stream   (car client))
         (client-socket   (cadr client))
         (request-verb    (car request))
         (request-path    (cadr request))
         (request-query   (caddr request))
        
         (filesystem-path (let ((p (pathname request-path)))
                            (setf (car (pathname-directory p)) :relative)
                            (merge-pathnames p *default-pathname-defaults*)))
         
         (range-start     nil)
         (range-end       nil))


    (labels ((date-header ()
               (let ((days   '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
                     (months '("x" "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")))

                 (multiple-value-bind
                   (sec min hour date month year day)
                   (decode-universal-time (get-universal-time) 0)

                   (format nil "Date: ~a, ~2,'0d ~a ~d ~2,'0d:~2,'0d:~2,'0d GMT~c~c"
                     (nth day days) date (nth month months) year hour min sec #\return #\linefeed))))


             (response (status &optional body binary content-type content-range)

               (let ((statuses '((200 . "OK")
                                 (206 . "Partial Content")
                                 (400 . "Bad Request")
                                 (403 . "Forbidden")
                                 (404 . "Not Found"))))

                 (format client-stream "HTTP/1.1 ~d ~a~c~c"
                   status (cdr (assoc status statuses)) #\return #\linefeed))

               (format client-stream (date-header))
               (format client-stream "Content-Length: ~d~c~c" (length body) #\return #\linefeed)

               (when content-type
                 (format client-stream "Content-Type: ~a~c~c" content-type #\return #\linefeed))

               (when content-range
                 (format client-stream "Content-Range: ~a~c~c" content-range #\return #\linefeed))

               (let ((ok (and (< status 300) (>= status 200))))
                 (format client-stream "Connection: ~a~c~c"
                   (if ok "keep-alive" "close") #\return #\linefeed)

                 (format client-stream "~c~c" #\return #\linefeed)
                 (finish-output client-stream)

                 (when body
                   (if binary
                     (sb-bsd-sockets:socket-send client-socket body (length body))
                     (progn
                       (write-sequence body client-stream)
                       (finish-output client-stream))))

                 (unless ok
                   (close client-stream))

                 (return-from http-server/seekable-handler)))
                 

             (response-lazy (status content-length body-stream binary
                             &optional content-type content-range)
               
               (let ((statuses '((200 . "OK") (206 . "Partial Content"))))
                 (format client-stream "HTTP/1.1 ~d ~a~c~c"
                   status (cdr (assoc status statuses)) #\return #\linefeed))

               (format client-stream (date-header))
               (format client-stream "Content-Length: ~d~c~c" content-length #\return #\linefeed)

               (when content-type
                 (format client-stream "Content-Type: ~a~c~c" content-type #\return #\linefeed))

               (when content-range
                 (format client-stream "Content-Range: ~a~c~c" content-range #\return #\linefeed))

               (format client-stream "Connection: keep-alive~c~c" #\return #\linefeed)
               (format client-stream "~c~c" #\return #\linefeed)
               (finish-output client-stream)

               (if binary

                 (let ((buf (make-array (* 1024 256) :element-type '(unsigned-byte 8))))
                   (loop for n = (read-sequence buf body-stream)
                     do (sb-bsd-sockets:socket-send client-socket buf n)
                        (when (< n (length buf))
                          (return))))

                 (let ((buf (make-array (* 1024 256))))
                   (loop for n = (read-sequence buf body-stream)
                     do (write-sequence buf client-stream :end n)
                        (when (< n (length buf))
                          (progn
                            (finish-output client-stream)
                            (return))))))
                      
               (return-from http-server/seekable-handler)))
                     

      (let ((header (cdr (assoc "Range" headers :test #'equal))))

        (block header-parsing
          (unless (equal (search "bytes=" header) 0)
            (return-from header-parsing))

          (let ((i (position #\= header))
                (j (position #\- header)))
            (unless (and i j (< i j))
              (return-from header-parsing))

            (let ((s (subseq header (1+ i) j))
                  (e (subseq header (1+ j))))

              (handler-case
                (setq range-start (parse-integer s))
                (parse-error (c)
                  (declare (ignore c))
                  (return-from header-parsing)))

              (handler-case
                (setq range-end (parse-integer e))
                (parse-error (c)
                  (declare (ignore c))
                  (return-from header-parsing)))))))

      (flet ((ip-to-string (ip)
               (reduce (lambda (a b)
                         (concatenate 'string a "." b))
                       (mapcar #'write-to-string (coerce ip 'list)))))

        (multiple-value-bind (ip port) (sb-bsd-sockets:socket-peername client-socket)
          (format t "~a:~d: ~a ~a ~a ~a ~a~%"
            (ip-to-string ip) port request-verb request-path request-query range-start range-end)))

      (unless (equal (string-upcase request-verb) "GET")
        (response 400))

      (unless (probe-file filesystem-path)
        (response 404))

      (flet ((path-under-basepath-p (path basepath)
               (= (search (directory-namestring basepath) (namestring path)) 0)))

        (unless (path-under-basepath-p filesystem-path *default-pathname-defaults*)
          (response 403)))

      (if (not (pathname-name filesystem-path))

        (let ((body-stream (make-string-output-stream))

              (sort-pred   #'string-lessp)
              (sort-key    #'cadr))

          (flet ((parse-query (query)
                   (labels ((decode-param (s)
                              (labels ((eater (l m)
                                (if (null l)
                                  m
                                  (case (car l)

                                    (#\%       (if (and (>= (length l) 3)
                                                        (digit-char-p (cadr l) 16)
                                                        (digit-char-p (caddr l) 16))

                                                 (eater (cdddr l) 
                                                        (cons
                                                          (code-char
                                                            (parse-integer
                                                              (coerce (list (cadr l) (caddr l))
                                                                      'string)
                                                              :radix 16))
                                                          m))
                                                  
                                                 (eater (cdr l) (cons (car l) m))))

                                    (#\+       (eater (cdr l) (cons #\space m)))
                                    (otherwise (eater (cdr l) (cons (car l) m)))))))

                                (coerce (reverse (eater (coerce s 'list) nil)) 'string)))
                   
                            (tokenizer (s l)
                              (if (= (length s) 0)
                                l
                                (let ((i (position #\& s)))
                                  (cond
                                    ((null i) (cons s l))
                                    ((= i 0)  (tokenizer (subseq s 1) l))
                                    (t        (tokenizer (subseq s (1+ i))
                                                         (cons (subseq s 0 i) l)))))))

                            (splitter (s)
                              (let ((i (position #\= s)))
                                (if i
                                  (cons (decode-param (subseq s 0 i))
                                        (decode-param (subseq s (1+ i))))
                                  (cons (decode-param s) nil)))))

                     (when query
                       (mapcar #'splitter (tokenizer (subseq query 1) nil)))))
          
                 (path-to-href (path)
                   (concatenate 'string "/"
                     (enough-namestring (namestring path)
                                        (namestring *default-pathname-defaults*))))

                 (path-basename (path)
                   (if (pathname-name path)

                     (if (pathname-type path)
                       (concatenate 'string (pathname-name path) "." (pathname-type path))
                       (pathname-name path))

                     (concatenate 'string (car (reverse (pathname-directory path))) "/")))

                 (write-entry (href name last-mod size)
                   (format body-stream "<tr><td><a href=\"~a\">~a</a></td>" href name)

                   (if last-mod
                     (format body-stream "<td>~a</td>" last-mod)
                     (format body-stream "<td></td>"))

                   (if size
                     (format body-stream "<td class=\"right\">~a</td>" size)
                     (format body-stream "<td></td>"))

                   (format body-stream "</tr>~%")))

            (format body-stream "<html>~%")

            (format body-stream "<head><style>~%")
            (format body-stream ".right { text-align: right; }~%")
            (format body-stream "</style></head>~%")

            (format body-stream "<body>~%")
            (format body-stream "<h1>Index of ~a</h1>~%" (namestring filesystem-path))

            (format body-stream "<table><tr>~%")

            (let* ((query     (parse-query request-query))
                   (sort-col  (cdr (assoc "c" query :test #'equal)))
                   (sort-dir  (cdr (assoc "s" query :test #'equal))))

              (or (equal sort-col "n") (equal sort-col "l") (equal sort-col "s") (setq sort-col "n"))
              (or (equal sort-dir "a") (equal sort-dir "d") (setq sort-dir "a"))

              (setq sort-pred (if (or (equal sort-col "n") (equal sort-col "l"))

                                (lambda (a b)
                                  (let ((p (if (equal sort-dir "a") #'string< #'string>)))
                                    (funcall p (string-downcase a) (string-downcase b))))

                                (lambda (a b)
                                  (let ((p (if (equal sort-dir "a") #'< #'>)))
                                    (funcall p (parse-integer a) (parse-integer b))))))

              (setq sort-key (cond ((equal sort-col "n") #'cadr)
                                   ((equal sort-col "l") #'caddr)
                                   ((equal sort-col "s") #'cadddr)))

              (format body-stream "<th><a href=\"~a?c=n&s=~a\">Name</a></th>~%"
                (path-to-href filesystem-path)
                (if (and (equal sort-col "n") (equal sort-dir "a")) "d" "a"))

              (format body-stream "<th><a href=\"~a?c=l&s=~a\">Last modified</th>~%"
                (path-to-href filesystem-path)
                (if (and (equal sort-col "l") (equal sort-dir "a")) "d" "a"))

              (format body-stream "<th><a href=\"~a?c=s&s=~a\">Size</th></tr>~%"
                (path-to-href filesystem-path)
                (if (and (equal sort-col "s") (equal sort-dir "a")) "d" "a")))

            (unless (equal filesystem-path *default-pathname-defaults*)
              (write-entry (path-to-href (truename (merge-pathnames filesystem-path "..")))
                           "Parent directory"
                           nil
                           nil))

            (mapc
              (lambda (entry) (apply #'write-entry entry))

              (sort
                (mapcar (lambda (path)
                          (let ((stat (sb-posix:stat (namestring path))))
                            (multiple-value-bind
                              (sec min hour date month year)
                              (decode-universal-time (sb-posix:stat-mtime stat))

                              (setq year (+ year 70))

                              (list
                                (path-to-href path)
                                (path-basename path)
                                (format nil "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
                                  year month date hour min sec)
                                (write-to-string (sb-posix:stat-size stat))))))

                        (directory (merge-pathnames filesystem-path (pathname "*.*"))))
                sort-pred
                :key sort-key))

            (format body-stream "</table></body></html>~%")

            (let ((body (get-output-stream-string body-stream)))
              (response 200 body nil "text/html"))))

        (let ((filesize     (sb-posix:stat-size (sb-posix:stat (namestring filesystem-path))))
              (content-type (let ((type (pathname-type filesystem-path)))
                              (if (equal type "mp4")
                                "video/mp4"
                                nil))))

          (with-open-file (file-stream filesystem-path :direction :input
                                                       :element-type '(unsigned-byte 8))
            (if range-start

              (let ((buf  (make-array (* 1024 256) :element-type '(unsigned-byte 8)))
                    (body (make-array 0 :element-type '(unsigned-byte 8) :fill-pointer 0)))

                (file-position file-stream range-start)

                (unless range-end
                  (setq range-end (min (+ range-start (* 1024 1024 5)) (1- filesize))))

                (let ((left (1+ (- range-end range-start))))
                  (loop for n = (read-sequence buf file-stream)
                    do (let ((o (min n left)))
                         (loop for i from 0 to (1- o)
                           do (vector-push-extend (aref buf i) body))
                       (setq left (- left o))
                       (when (= left 0)
                         (return)))))

                (let ((range (format nil "bytes ~d-~d/~d" range-start range-end filesize)))
                  (response 206 body t content-type range)))

              (response-lazy 200 filesize file-stream t content-type))))))))


(http-server/serve #'http-server/seekable-handler)

