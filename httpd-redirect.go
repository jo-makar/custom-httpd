// Http server that redirects to itself endlessly

package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"os"
	"os/signal"
	"strconv"
	"syscall"
	"time"
)

func main() {
	const httpPort = 8000
	// Options: StatusMovedPermanently (301), StatusFound (302), StatusSeeOther (303), StatusTemporaryRedirect (307), StatusPermanentRedirect (308)
	const redirectCode = http.StatusMovedPermanently

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

		i := 0
		if v, ok := r.URL.Query()["i"]; ok {
			for _, w := range v {
				if x, err := strconv.Atoi(w); err == nil && x >= 0 {
					i = x
					break
				}
			}
		}
		i++

		url := r.URL
		q := url.Query()
		q.Set("i", strconv.Itoa(i))
		url.RawQuery = q.Encode()

		time.Sleep(1 * time.Second)
		log.Printf("%s: %s: redirecting to %s", r.RemoteAddr, r.RequestURI, url.String())
		http.Redirect(w, r, url.String(), redirectCode)
	})

	server := http.Server{
		        Addr: fmt.Sprintf(":%d", httpPort),
		 ReadTimeout: 10 * time.Second,
		WriteTimeout: 10 * time.Second,
	}
	log.Printf("will listen on port %d", httpPort)

	sigchan := make(chan os.Signal)
	signal.Notify(sigchan, syscall.SIGINT, syscall.SIGTERM)

	go func () {
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
