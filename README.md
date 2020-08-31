# custom-httpds

Custom http servers

## httpd-seekable.py

Seekable http server (via minimally invasive changes to SimpleHTTPRequestHandler)

Intended to be a drop-in alternative to "python3 -m http.server" for streaming media

## httpd-slow.go

Http server that (maliciously) throttles download speeds

## httpd-redirect.go

Http server that redirects to itself endlessly
