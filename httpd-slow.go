// Http server that (maliciously) throttles download speeds

package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"os"
	"os/signal"
	"syscall"
	"time"
)

func main() {
	const httpPort = 8000
	const videoPath = "tmp/180607_A_090.mp4"
	const contentType = "video/mp4"
	const batchSize = 128

	var video []byte
	if file, err := os.Open(videoPath); err != nil {
		log.Panic(err)
	} else {
		if video, err = ioutil.ReadAll(file); err != nil {
			log.Panic(err)
		}
		log.Printf("read %d bytes from %s", len(video), videoPath)
	}

	// http.{Handle,HandleFunc}() registers handlers in http.DefaultServeMux (used by http.Server below)
	http.HandleFunc("/video.mp4", func(w http.ResponseWriter, r *http.Request) {

		//
		// Request logging
		//

		if body, err := ioutil.ReadAll(r.Body); err != nil {
			// Should never happen
			log.Printf("%s %s %s %s %s %v", r.RemoteAddr, r.Method, r.Host, r.RequestURI, r.Proto, r.Header)
			log.Panic(err)
		} else {
			log.Printf("%s %s %s %s %s %v %q", r.RemoteAddr, r.Method, r.Host, r.RequestURI, r.Proto, r.Header, body)
		}

		if r.Method != "GET" {
			log.Printf("%s: %s: unsupported method (%s)", r.RemoteAddr, r.RequestURI, r.Method)
			w.WriteHeader(http.StatusBadRequest)
			return
		}

		//
		// http.Hijack() to control the response header
		// Specifically the Content-Length header
		//

		h, ok := w.(http.Hijacker)
		if !ok {
			log.Panic("Hijacker interface not implemented")
		}
		conn, rw, err := h.Hijack()
		if err != nil {
			log.Panic("%s: %s: %s", r.RemoteAddr, r.RequestURI, err.Error())
		}
		defer conn.Close()

		writeString := func(s string) {
			if _, err := rw.WriteString(s); err != nil {
				log.Panic("%s: %s: %s", r.RemoteAddr, r.RequestURI, err.Error())
			}
		}

		writeBytes := func(b []byte) {
			if _, err := rw.Write(b); err != nil {
				log.Panic("%s: %s: %s", r.RemoteAddr, r.RequestURI, err.Error())
			}
		}

		flush := func() {
			if err := rw.Flush(); err != nil {
				log.Panic("%s: %s: %s", r.RemoteAddr, r.RequestURI, err.Error())
			}
		}

		//
		// Response header
		//

		writeString("HTTP/1.1 200 OK\r\n")
		writeString(fmt.Sprintf("Content-Type: %s\r\n", contentType))
		writeString(fmt.Sprintf("Date: %s\r\n", time.Now().Format("Mon, 2 Jan 2006 15:04:05 MST")))
		writeString("Transfer-Encoding: chunked\r\n")
		writeString(fmt.Sprintf("Content-Length: %d\r\n", len(video))) // TODO Necessary?
		writeString("\r\n")
		flush()

		//
		// Response body
		// Ref: https://en.wikipedia.org/wiki/Chunked_transfer_encoding
		//

		buf := make([]byte, batchSize)
		for i, b := range video {
			buf[i % batchSize] = b

			if i % batchSize == batchSize - 1 {
				log.Printf("%s: %s: sending batch for bytes %d - %d", r.RemoteAddr, r.RequestURI, i - (batchSize-1), i)
				writeString(fmt.Sprintf("%x\r\n", batchSize))
				writeBytes(buf)
				writeString("\r\n")
				flush()

				time.Sleep(1 * time.Second)

			} else if i == len(video) - 1 {
				n := (i % batchSize) + 1
				log.Printf("%s: %s: sending final batch for bytes %d - %d", r.RemoteAddr, r.RequestURI, i - (n-1), i)
				writeString(fmt.Sprintf("%x\r\n", n))
				writeBytes(buf[:n])
				writeString("\r\n")
				flush()
			}
		}

		writeString("0\r\n")
		writeString("\r\n")
		flush()
	})

	server := http.Server{
		        Addr: fmt.Sprintf(":%d", httpPort),
		 ReadTimeout: 10 * time.Second,
		WriteTimeout: 10 * time.Second,
	}
	log.Printf("will listen on port %d", httpPort)

	sigchan := make(chan os.Signal)
	signal.Notify(sigchan, syscall.SIGINT, syscall.SIGTERM)

	go func() {
		s := <-sigchan
		log.Printf("terminating signal received (%d)", s)
		// This causes server.ListenAndServe() to return with http.ErrServerClosed
		server.Close()
	}()

	if err := server.ListenAndServe(); err != nil {
		if err != http.ErrServerClosed {
			log.Panic(err)
		}
	}
}
