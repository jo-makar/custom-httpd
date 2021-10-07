# custom-httpds

Custom http servers

## httpd-post.py

Http server with (file) post support

Intended client use: `curl -F 'file=@/path/to/file' ...`

## httpd-redirect.go

Http server that redirects to itself endlessly

## httpd-seekable.py

Seekable http server (via minimally invasive changes to SimpleHTTPRequestHandler)

Intended to be a drop-in alternative to "python3 -m http.server" for streaming media

## httpd-slow.go

Http server that (maliciously) throttles download speeds

## http-server.lisp

Simple http server implemented in Common Lisp
