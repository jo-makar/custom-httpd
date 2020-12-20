#!/usr/bin/env python3
# Http server with (file) post support
# Intended client use: curl -F 'file=@/path/to/file' ...
#
# Supports https with the -t flag:
#     Generate a self-signed certificate and private key:
#         Note that public (but not private) ips can and should be used for the certificate common name.
#         openssl req -x509 -nodes -newkey rsa:4096 -keyout server.key -out server.crt -subj '/CN=127.0.0.1'
#     Specify the certificate with --cacert to allow curl to verify the server.

import argparse, http.client, http.server, ssl

class Handler(http.server.BaseHTTPRequestHandler):
    # Ref: https://github.com/python/cpython/blob/3.8/Lib/http/server.py

    def do_POST(self):
        def respond(code, type='text/html', resp=b''):
            if not resp:
                if code in http.client.responses:
                    resp = bytes(f'<html><body><h1>{code} {http.client.responses[code]}</h1></body></html>\n', encoding='utf-8')

            self.send_response(code)
            self.send_header('Content-Type', type)
            self.send_header('Content-Length', str(len(resp)))
            self.end_headers()

            if len(resp):
                self.wfile.write(resp)

        if not self.headers['Content-Type'].startswith('multipart/form-data; boundary=') or \
           not self.headers['Content-Length'] or \
           self.headers['Expect'] != '100-continue':

            respond(400)
            return
        
        length = int(self.headers['Content-Length'])
        boundary = self.headers['Content-Type'][len('multipart/form-data; boundary='):]

        self.send_response_only(100)
        self.end_headers()

        # TODO The following assumes only a single form entry.
        #      Otherwise will need to check line by line for boundaries.

        line = self.rfile.readline()
        length -= len(line)
        assert line.decode('utf-8').strip() == '--' + boundary

        line = self.rfile.readline()
        length -= len(line)
        assert line.decode('utf-8').startswith('Content-Disposition:')

        while True:
            line = self.rfile.readline()
            length -= len(line)
            if line.decode('utf-8') == '\r\n':
                break

        with open('httpd-post-file', 'wb') as output:
            # The data is terminated by a boundary and new lines
            limit = 8 + len(boundary)

            while length > limit:
                n = 4096 if length - limit > 4096 else (length - limit)
                chunk = self.rfile.read(n)
                if chunk == b'':
                    break
                output.write(chunk)
                length -= len(chunk)

        respond(200)

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('port', type=int, nargs='?', default=8000)
    parser.add_argument('--bind', '-b', default='0.0.0.0')
    parser.add_argument('--tls', '-t', action='store_true')
    args = parser.parse_args()

    httpd = http.server.HTTPServer((args.bind, args.port), Handler)
    if args.tls:
        httpd.socket = ssl.wrap_socket (httpd.socket, certfile='./server.crt', keyfile='./server.key', server_side=True)
    httpd.serve_forever()

