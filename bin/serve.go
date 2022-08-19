package main

import (
	"flag"
	"fmt"
	"io/ioutil"
	"net/http"
	"strings"
)

func listDir(w http.ResponseWriter, req *http.Request) {
	files, err := ioutil.ReadDir(".")
	if err != nil {
		fmt.Printf("error reading dir: %s\n", err)
		fmt.Fprintf(w, "*** error reading dir")
		return
	}
	w.Header().Set("Content-Type", "text/html")
	for _, f := range files {
		fmt.Fprintf(w, "<a href='%s'>%s</a><br/>", f.Name(), f.Name())
	}
}

func serveFile(filename string, w http.ResponseWriter, req *http.Request) {
	files, err := ioutil.ReadDir(".")
	if err != nil {
		fmt.Fprintf(w, "*** error reading file")
		return
	}
	for _, f := range files {
		if f.Name() == filename {
			bytes, err := ioutil.ReadFile(f.Name())
			if err != nil {
				fmt.Fprintf(w, "*** error reading file")
				return
			}

			if *sendCorsHeaders {
				w.Header().Set("Access-Control-Allow-Credentials", "true")
				w.Header().Set("Access-Control-Allow-Origin", "*")
				w.Header().Set("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
				w.Header().Set("Access-Control-Allow-Headers", "X-Requested-With, Content-type")
			}

			w.Header().Set("Content-Type", http.DetectContentType(bytes))
			w.Write(bytes)
			return
		}
	}
	fmt.Fprintf(w, "file not found: %s", filename)
}

func serve(w http.ResponseWriter, req *http.Request) {
	// FIXME, eg: 127.0.0.1 - - [18/Aug/2022 21:23:06] "GET / HTTP/1.1" 200 -
	fmt.Printf("[serving request %s %s]\n", req.Method, req.URL.Path)

	if req.URL.Path == "/" {
		listDir(w, req)
	} else {
		requestedFile := req.URL.Path[1:]
		if strings.Contains(requestedFile, "..") ||
			strings.ContainsAny(requestedFile, "\\/") {
			fmt.Fprintf(w, "invalid file name")
		} else {
			serveFile(requestedFile, w, req)
		}
	}
}

var sendCorsHeaders = flag.Bool("cors", false, "send cors headers")

func main() {
	port := flag.String("port", "8090", "set the port")
	flag.Parse()

	fmt.Printf("[serving on port %s]\n", *port)
	if *sendCorsHeaders {
		fmt.Printf("[sending cors headers]\n")
	}

	http.HandleFunc("/", serve)
	http.ListenAndServe(":"+*port, nil)
}
