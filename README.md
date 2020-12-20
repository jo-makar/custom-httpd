# custom-httpds

Custom http servers

## httpd-post.py

Http server with (file) post support

Intended client use: `curl -F 'file=@/path/to/file' ...`

Supports https with the -t flag:
- Generate a self-signed certificate and private key:
  - Note that public (but not private) ips can and should be used for the certificate common name
  - `openssl req -x509 -nodes -newkey rsa:4096 -keyout server.key -out server.crt -subj '/CN=127.0.0.1'`
- Specify the certificate with --cacert to allow curl to verify the server

## httpd-redirect.go

Http server that redirects to itself endlessly

## httpd-seekable.py

Seekable http server (via minimally invasive changes to SimpleHTTPRequestHandler)

Intended to be a drop-in alternative to "python3 -m http.server" for streaming media

## httpd-slow.go

Http server that (maliciously) throttles download speeds
