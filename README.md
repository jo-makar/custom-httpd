# custom-httpds

Custom http servers

## httpd-log.py

Http server that logs requests

## httpd-post.py

Http server with (file) post support

Intended client use: `curl -F 'file=@/path/to/file' ...`

## httpd-redirect.go

Http server that redirects to itself endlessly

## httpd-seekable.py

Seekable http server (via minimally invasive changes to SimpleHTTPRequestHandler)

Intended to be a drop-in alternative to "python3 -m http.server" for streaming media

## httpd-seekable.lisp

Another seekable http server (developed against SBCL)

Index view supports sorting by filename, size and date

## httpd-slow.go

Http server that (maliciously) throttles download speeds
