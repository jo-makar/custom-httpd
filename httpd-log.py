#!/usr/bin/env python3
# Http server that logs requests

import argparse
import http.server
import logging
import ssl

class Handler(http.server.BaseHTTPRequestHandler):
    def respond(self):
        self.send_response(200)
        self.end_headers()

    def do_GET(self):
        self.respond()
    
    def do_POST(self):
        self.respond()

    def log_request(self, code='-', size='-'):
        if self.command == 'POST':
            body = self.rfile.read(int(self.headers['Content-Length']))
            logging.info(f'"{self.requestline}" {list(self.headers.items())} {body}')
        else:
            logging.info(f'"{self.requestline}" {list(self.headers.items())}')

if __name__ == '__main__':
    logging.basicConfig(level=logging.INFO, format='%(asctime)s %(message)s')

    parser = argparse.ArgumentParser()
    parser.add_argument('port', type=int, nargs='?', default=8000)
    parser.add_argument('--bind', '-b', default='0.0.0.0')
    parser.add_argument('--tls', '-t', action='store_true')
    args = parser.parse_args()

    httpd = http.server.HTTPServer((args.bind, args.port), Handler)

    if args.tls:
        context = ssl.create_default_context(ssl.Purpose.CLIENT_AUTH)
        context.load_cert_chain(certfile='./server.crt', keyfile='./server.key')
        httpd.socket = context.wrap_socket(httpd.socket, server_side=True)

    httpd.serve_forever()
