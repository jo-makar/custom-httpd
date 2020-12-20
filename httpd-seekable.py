#!/usr/bin/env python3
# Seekable http server (via minimally invasive changes to SimpleHTTPRequestHandler)
# Intended to be a drop-in alternative to "python3 -m http.server" for streaming media

import argparse, http.server, os, re

class Handler(http.server.SimpleHTTPRequestHandler):
    # Ref: https://github.com/python/cpython/blob/3.8/Lib/http/server.py

    def do_GET(self):
        try:
            if 'Range' not in self.headers:
                super().do_GET()

            else:
                match = re.match(r'^bytes=(\d+)-(\d+)?$', self.headers['Range'])
                assert match is not None
                self._range_start = int(match.group(1))

                self._filesize = os.path.getsize(self.translate_path(self.path))
                if match.group(2) is None or int(match.group(2)) >= self._filesize:
                    self._range_stop = self._filesize - 1
                assert self._range_stop > self._range_start

                f = self.send_head()
                if f:
                    try:
                        # TODO What's an appropriate value here?
                        #      Consider measuring throughput with different values?
                        buflen = 64 * 1024

                        f.seek(self._range_start)
                        count = 1 + self._range_stop - self._range_start
                        while count > 0:
                            n = min(buflen, count)
                            self.wfile.write(f.read(n))
                            count -= n
                    finally:
                        f.close()

        # These are generally caused by the browser media player seeking to another point in the media file
        except BrokenPipeError:
            pass
        except ConnectionResetError:
            pass

    def end_headers(self):
        if hasattr(self, '_range_start') and hasattr(self, '_range_stop'):
            assert self._headers_buffer[0] == b'HTTP/1.0 200 OK\r\n'
            self._headers_buffer[0] = b'HTTP/1.0 206 OK\r\n'

            self._headers_buffer = [h for h in self._headers_buffer if not h.startswith(b'Content-Length:')]
            self.send_header('Content-Range', 'bytes {}-{}/{}'.format(self._range_start, self._range_stop, self._filesize))
            self.send_header('Content-Length', 1 + self._range_stop - self._range_start)

        super().end_headers()

    def log_request(self, code='-', size='-'):
        if code != 200 or os.path.isdir(self.translate_path(self.path)):
            super().log_request(code, size)
        elif hasattr(self, '_range_start') and hasattr(self, '_range_stop'):
            super().log_request(code, '{} - {} / {}'.format(self._range_start,
                                                            self._range_stop,
                                                            1 + self._range_stop - self._range_start))
        else:
            super().log_request(code, os.path.getsize(self.translate_path(self.path)))

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('port', type=int, nargs='?', default=8000)
    parser.add_argument('--bind', '-b', default='0.0.0.0')
    args = parser.parse_args()

    http.server.ThreadingHTTPServer((args.bind, args.port), Handler).serve_forever()
