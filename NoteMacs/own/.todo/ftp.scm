;;; ftp.scm -- an FTP client library for the Scheme Shell
;;
;; $Id: ftp.scm,v 1.1 1998/04/29 06:54:13 ecm Exp $
;;
;;     Copyright (C) 1998  Eric Marsden
;;   
;;     This library is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU Library General Public
;;     License as published by the Free Software Foundation; either
;;     version 2 of the License, or (at your option) any later version.
;;   
;;     This library is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;     Library General Public License for more details.
;;   
;;     You should have received a copy of the GNU Library General Public
;;     License along with this library; if not, write to the Free
;;     Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;; Please send suggestions and bug reports to <emarsden@mail.dotcom.fr>



;;; Overview =========================================================
;;
;; This module lets you transfer files between networked machines from
;; the Scheme Shell, using the File Transfer Protocol as described
;; in rfc959. The protocol specifies the behaviour of a server
;; machine, which runs an ftp daemon (not implemented by this module),
;; and of clients (that's us) which request services from the server.


;;; Entry points =======================================================
;;
;; (ftp:connect host [logfile]) -> connection
;;     Open a command connection with the remote machine HOST.
;;     Optionally start logging the conversation with the server to
;;     LOGFILE, which will be appended to if it already exists, and
;;     created otherwise. Beware, the LOGFILE contains passwords in
;;     clear text (it is created with permissions og-rxw) !
;;
;; (ftp:login connection [login passwd]) -> status
;;     Log in to the remote host. If a login and password are not
;;     provided, they are first searched for in the user's ~/.netrc
;;     file, or default to user "anonymous" and password "user@host"
;;
;; (ftp:type connection type) -> status
;;     Change the transfer mode for future data connections. This may
;;     be either 'ascii, for transfering text files, or 'binary for
;;     transfering binary files. If type is a string it is sent
;;     verbatim to the server.
;;
;; (ftp:rename connection oldname newname) -> status
;;     Change the name of oldname on the remote host to newname
;;     (assuming sufficient permissions). oldname and newname are
;;     strings; if prefixed with "/" they are taken relative to the
;;     server's root, and otherwise they are relative to the current
;;     directory. Note that in the case of anonymous ftp (user
;;     "anonymous" or "ftp"), the server root is different from the
;;     root of the servers's filesystem.
;;
;; (ftp:delete connection file) -> status
;;     Delete file from the remote host (assuming the user has
;;     appropriate permissions).
;;
;; (ftp:cd connection dir) -> status
;;     Change the current directory on the server.
;;
;; (ftp:cdup connection) -> status
;;     Move to the parent directory on the server.
;;
;; (ftp:pwd connection) -> string
;;     Return the current directory on the remote host, as a string.
;;
;; (ftp:ls connection) -> status
;;     Provide a listing of the current directory's contents, in short
;;     format, ie as a list of filenames.
;;
;; (ftp:dir connection) -> status
;;     Provide a listing of the current directory's contents, in long
;;     format. Most servers (Unix, MS Windows, MacOS) use a standard
;;     format with one file per line, with the file size and other
;;     information, but other servers (VMS, ...) use their own format.
;;
;; (ftp:get connection remote-file [local-file]) -> status | string
;;     Download remote-file from the FTP server. If local-file is a
;;     string, save the data to local-file on the local host;
;;     otherwise save to a local file named remote-file. remote-file
;;     and local-file may be absolute file names (with a leading `/'),
;;     or relative to the current directory. It local-file is #t,
;;     output data to (current-output-file), and if it is #f return
;;     the data as a string.
;;
;; (ftp:put connection local-file [remote-file]) -> status
;;     Upload local-file to the FTP server. If remote-file is
;;     specified, the save the data to remote-file on the remote host;
;;     otherwise save to a remote file named local-file. local-file
;;     and remote-file may be absolute file names (with a leading
;;     `/'), or relative to the current directory.
;;
;; (ftp:rmdir connection dir) -> status
;;     Remove the directory DIR from the remote host (assuming
;;     sufficient permissions).
;;
;; (ftp:mkdir connection dir) -> status
;;     Create a new directory named DIR on the remote host (assuming
;;     sufficient permissions).
;;
;; (ftp:modification-time connection file) -> date
;;     Request the time of the last modification of FILE on the remote
;;     host, and on success return a Scsh date record. This command is
;;     not part of RFC959 and is not implemented by all servers, but
;;     is useful for mirroring.
;;
;; (ftp:size connection file) -> integer
;;     Return the size of FILE in bytes.
;;
;; (ftp:abort connection) -> status
;;     Abort the current data transfer. Not particularly useful with
;;     this implementation since the data transfer commands only
;;     return once the transfer is complete.
;;
;; (ftp:quit connection) -> status
;;     Close the connection to the remote host. The connection object
;;     is useless after a quit command.


;;; Unimplemented =====================================================
;;
;; This module has no support for sites behind a firewall (because I
;; am unable to test it). It shouldn't be very tricky; it only
;; requires using passive mode. Might want to add something like the
;; /usr/bin/ftp command `restrict', which implements data port range
;; restrictions.
;;
;; The following rfc959 commands are not implemented:
;;
;; * ACCT (account; this is ignored by most servers)
;; * SMNT (structure mount, for mounting another filesystem)
;; * REIN (reinitialize connection)
;; * LOGOUT (quit without interrupting ongoing transfers)
;; * STRU (file structure)
;; * ALLO (allocate space on server)


;;; Portablitity =====================================================
;;
;; * the netrc.scm module for parsing ~/.netrc files
;; * scsh socket code
;; * scsh records
;; * receive for multiple values
;; * Scheme48 signals/handlers


;;; Related work ======================================================
;;
;; * rfc959 describes the FTP protocol; see
;;   http://www.cis.ohio-state.edu/htbin/rfc/rfc959.html
;;
;; * /anonymous@sunsite.unc.edu:/pub/Linux/libs/ftplib.tar.gz is a
;;   library similar to this one, written in C, by Thomas Pfau
;;
;; * FTP.pm is a Perl module with similar functionality (available
;;   from http://www.perl.com/CPAN)
;;
;; * Emacs gets transparent remote file access from ange-ftp.el by
;;   Ange Norman. However, it cheats by using /usr/bin/ftp
;;
;; * Siod (a small-footprint Scheme implementation by George Carette)
;;   comes with a file ftp.scm with a small subset of these functions
;;   defined


;;; TODO ============================================================
;;
;; * handle passive mode and firewalls
;; * Unix-specific commands such as SITE UMASK, SITE CHMOD
;; * object-based interface? (like SICP message passing)
;; * improved error handling
;; * a lot of the calls to format could be replaced by calls to
;;   join-strings. Maybe format is easier to read?



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Communication is initiated by the client. The server responds to
;; each request with a three digit status code and an explanatory
;; message, and occasionally with data (which is sent via a separate,
;; one-off channel). The client starts by opening a command connection
;; to a well known port on the server machine. Messages send to the
;; server are of the form
;;
;;          CMD [ <space> arg ] <CR> <LF>
;;
;; Replies from the server are of the form
;;
;;          xyz <space> Informative message <CR> <LF>
;;
;; where xyz is a three digit code which indicates whether the
;; operation succeeded or not, whether the server is waiting for more
;; data, etc. The server may also send multiline messages of the form
;;
;;          xyz- <space> Start of multiline message <CR> <LF>
;;          [ <space>+ More information ]* <CR> <LF>
;;          xyz <space> End of multiline message <CR> <LF>
;;
;; Some of the procedures in this module extract useful information
;; from the server's reply, such as the size of a file, or the name of
;; the directory we have moved to. These procedures return either the
;; extracted information, or #f to indicate failure. Other procedures
;; return a "status", which is either the server's reply as a string,
;; or #f to signify failure.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; beware, the log file contains password information!
;;: string [ x string ] -> connection
(define (ftp:connect host . maybe-logfile)
  (let* ((logfile (safe-first maybe-logfile))
         (LOG (and logfile
                   (open-output-file logfile
                                     (if (file-exists? logfile)
                                         (bitwise-ior open/write open/append)
                                         (bitwise-ior open/write open/create))
                                     #o600)))
         (hst-info (host-info host))
         (hostname (host-info:name hst-info))
         (srvc-info (service-info "ftp" "tcp"))
         (sock (socket-connect protocol-family/internet
                               socket-type/stream
                               hostname
                               (service-info:port srvc-info)))
         (connection (make-ftp-connection hostname
                                          sock
                                          LOG "" "")))
    (ftp:log connection
             (format #f "~%-- ~a: opened ftp connection to ~a"
                     ;; (date->string (date)) 
                     "Dummy date"       ; (format-time-zone) is buggy in v0.5.1
                     hostname))
    (ftp:read-response connection "220") ; the initial welcome banner
    connection))

;; Send user information to the remote host. Args are optional login
;; and password. If they are not provided, the Netrc module is used to
;; try to determine a login and password for the server. If not found we
;; default to login "anonymous" with password user@host.
;;: connection [ x string x password ] -> status
(define (ftp:login connection . args)
  (let ((login (or (safe-first args)
                   ;; (netrc:lookup-login (ftp-connection:host-name connection))
                   "anonymous"))
        (password (or (safe-second args)
                      ;;(netrc:lookup-password
                      ;; (ftp-connection:host-name connection))
                      (user-mail-address))))
    (set-ftp-connection:login connection login)
    (set-ftp-connection:password connection password)
    (ftp:send-command connection (format #f "USER ~a" login) "...")  ; "331"
    (ftp:send-command connection (format #f "PASS ~a" password) "2.."))) ; "230"

;; Type must be one of 'binary or 'text, or a string which will be
;; sent verbatim
;;: connection x symbol|string -> status
(define (ftp:type connection type)
  (let ((ttype (cond
          ((string? type) type)
          ((eq? type 'binary) "I")
          ((eq? type 'text)   "A")
          (else
           (call-error "type must be one of 'binary or 'text" ftp:type type)))))
    (ftp:send-command connection (format #f "TYPE ~a" ttype))))

;;: connection x string x string -> status
(define (ftp:rename connection oldname newname)
  (ftp:send-command connection (format #f "RNFR ~a" oldname) "35.")
  (ftp:send-command connection (format #f "RNTO ~a" newname) "25."))

;;: connection x string -> status
(define (ftp:delete connection file)
  (ftp:send-command connection (format #f "DELE ~a" file) "25."))

;;: connection x string -> status
(define (ftp:cd connection dir)
  (ftp:send-command connection (format #f "CWD ~a" dir)))

;;: connection -> status
(define (ftp:cdup connection)
  (ftp:send-command connection "CDUP" "250"))


;;: on success return the new directory as a string
(define (ftp:pwd connection)
  (let* ((response (ftp:send-command connection "PWD" "2..")) ;; 257
         (match (string-match "[0-9][0-9][0-9] \"(.*)\" " (or response ""))))
    (match:substring match 1)))

;;: connection x string -> status
(define (ftp:rmdir connection dir)
  (ftp:send-command connection (format #f "RMD ~a" dir)))

;;: connection x string -> status
(define (ftp:mkdir connection dir)
  (ftp:send-command connection (format #f "MKD ~a" dir)))

;; On success return a Scsh date record. This message is not part of
;; rfc959 but seems to be supported by many ftp servers (it's useful
;; for mirroring)
;;: connection x string -> date
(define (ftp:modification-time connection file)
  (let* ((response (ftp:send-command connection
                                     (format #f "MDTM ~a" file)))
         (match (string-match "[0-9][0-9][0-9] ([0-9]+)" (or response "")))
         (timestr (and match (match:substring match 1))))
    (and timestr
         (let ((year  (substring timestr 0 4))
               (month (substring timestr 4 6))
               (mday  (substring timestr 6 8))
               (hour  (substring timestr 8 10))
               (min   (substring timestr 10 12))
               (sec   (substring timestr 12 14)))
           (make-date (string->number sec)
                      (string->number min)
                      (string->number hour)
                      (string->number mday)
                      (string->number month)
                      (- (string->number year) 1900))))))

;; On success return the size of the file in bytes.
;;: connection x string -> integer
(define (ftp:size connection file)
  (let* ((response (ftp:send-command connection
                                     (format #f "SIZE ~a" file)
                                     "2..")))
    (and (string? response)
         (string->number (substring response
                                    4 (- (string-length response) 1))))))

;; Abort the current data transfer. Maybe we should close the data
;; socket?
;;: connection -> status
(define (ftp:abort connection)
  (ftp:send-command connection "ABOR"))

;;: connection -> status
(define (ftp:quit connection)
  (ftp:send-command connection "QUIT" "221")
  (close-socket (ftp-connection:command-socket connection)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The following commands require the use of a data connection as well
;; as the command connection. The command and the server's reply are
;; transmitted via the command connection, while the data is
;; transmitted via the data connection (you could have guessed that,
;; right?).
;;
;; The data socket is created by the client, who sends a PORT command
;; to the server to indicate on which port it is ready to accept a
;; connection. The port command specifies an IP number and a port
;; number, in the form of 4+2 comma-separated bytes. The server then
;; initiates the data transfer. A fresh data connection is created for
;; each data transfer (unlike the command connection which stays open
;; during the entire conversation with the server).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;: connection [ x string ] -> status
(define (ftp:ls connection . maybe-dir)
  (let* ((sock (ftp:open-data-connection connection)))
    (ftp:send-command connection
                      (ftp:build-command-string "NLST" maybe-dir)
                      "1..")
    (receive (newsock newsockaddr)
             (accept-connection sock)
             (dump (socket:inport newsock))
             (let ((status (ftp:read-response connection "2..")))
               (close-socket newsock)
               (close-socket sock)
               status))))

;;: connection [ x string ] -> status
(define (ftp:dir connection . maybe-dir)
  (let* ((sock (ftp:open-data-connection connection)))
    (ftp:send-command connection
                      (ftp:build-command-string "LIST" maybe-dir)
                      "1..")
    (receive (newsock newsockaddr)
             (accept-connection sock)
             (dump (socket:inport newsock))
             (let ((status (ftp:read-response connection "2..")))
               (close-socket newsock)
               (close-socket sock)
               status))))


;; maybe-local may be a filename to which the data should be written,
;; or #t to write data to stdout (to current-output-port to be more
;; precise), or #f to stuff the data in a string (which is returned),
;; or nothing to output to a local file with the same name as the
;; remote file.
;;: connection x string [x string | #t | #f] -> status | string
(define (ftp:get connection remote-file . maybe-local)
  (let* ((sock (ftp:open-data-connection connection))
         (local (if (pair? maybe-local)
                    (car maybe-local)
                    'empty))
         (OUT (cond ((string? local) (open-output-file local))
                    ((eq? local #t) (current-output-port))
                    ((eq? local #f) (make-string-output-port))
                    (else
                     (open-output-file remote-file)))))
    (ftp:send-command connection
                      (format #f "RETR ~a" remote-file)
                      "150")
    (receive (newsock newsockaddr)
             (accept-connection sock)
             (with-current-output-port OUT
                (dump (socket:inport newsock)))
             (let ((status (ftp:read-response connection "2..")))
               (if (string? local) (close OUT))
               (close-socket newsock)
               (close-socket sock)
               (if (eq? local #f)
                   (string-output-port-output OUT)
                   status)))))


;; FIXME: should have an optional argument :rename which defaults to
;; false, which would make us upload to a temporary name and rename at
;; the end of the upload. This atomicity is important for ftp or http
;; servers which are serving a load, and to avoid problems with "no
;; space on device".

;; optional argument maybe-remote-file is the name under which we wish
;; the file to appear on the remote machine. If omitted the file takes
;; the same name on the FTP server as on the local host.
;;: connection x string [ x string ] -> status
(define (ftp:put connection local-file . maybe-remote-file)
  (let* ((sock (ftp:open-data-connection connection))
         (remote-file (safe-first maybe-remote-file))
         (IN (open-input-file local-file))
         (cmd (format #f "STOR ~a" (or remote-file local-file))))
    (ftp:send-command connection cmd "150")
    (receive (newsock newsockaddr)
             (accept-connection sock)
             (with-current-output-port (socket:outport newsock) (dump IN))
             (close (socket:outport newsock)) ; send the server EOF
             (close-socket newsock)
             (let ((status (ftp:read-response connection "2..")))
               (close IN)
               (close-socket sock)
               status))))

;;: connection x string [x string] -> status
(define (ftp:append connection local-file . remote-file)
  (let* ((sock (ftp:open-data-connection connection))
         (remote-file (safe-first maybe-remote-file))
         (IN (open-input-file local-file))
         (cmd (format #f "APPE ~a" (or remote-file local-file))))
    (ftp:send-command connection cmd "150")
    (receive (newsock newsockaddr)
             (accept-connection sock)
             (with-current-output-port (socket:outport newsock)
                (dump IN))
             (close (socket:outport newsock)) ; send the server EOF
             (close-socket newsock)
             (let ((status (ftp:read-response connection "2..")))
               (close IN)
               (close-socket sock)
               status))))

;; send a command verbatim to the remote server and wait for a
;; response.
;;: connection x string -> status
(define (ftp:quot connection cmd)
  (ftp:send-command connection cmd))


;; ------------------------------------------------------------------------
;; no exported procedures below

(define (ftp:open-data-connection connection)
  (let* ((sock (create-socket protocol-family/internet
                              socket-type/stream))
         (sockaddr (internet-address->socket-address
                    internet-address/any
                    0)))                ; 0 to accept any port
    (set-socket-option sock level/socket socket/reuse-address #t)
    (set-socket-option sock level/socket socket/linger 120)
    (bind-socket sock sockaddr)
    (listen-socket sock 0)
    (ftp:send-command connection        ; send PORT command
                      (ftp:build-PORT-string (socket-local-address sock)))
    sock))



;; TODO: Unix-specific commands
;; SITE UMASK 002
;; SITE IDLE 60
;; SITE CHMOD 755 filename
;; SITE HELP



;; We cache the login and password to be able to relogin automatically
;; if we lose the connection (a la ange-ftp). Not implemented.
(define-record ftp-connection
  host-name
  command-socket
  logfd
  login
  password)

(define-condition-type 'ftp:error '(error))
(define ftp:error? (condition-predicate 'ftp:error))


(define (ftp:build-PORT-string sockaddr)
  (let* ((hst-info (host-info (system-name)))
         (ip-address (car (host-info:addresses hst-info))))
  (receive (hst-address srvc-port)
           (socket-address->internet-address sockaddr)
           (let* ((num32 ip-address)
                  (num24 (arithmetic-shift num32 -8))
                  (num16 (arithmetic-shift num24 -8))
                  (num08 (arithmetic-shift num16 -8))
                  (byte0 (bitwise-and #b11111111 num08))
                  (byte1 (bitwise-and #b11111111 num16))
                  (byte2 (bitwise-and #b11111111 num24))
                  (byte3 (bitwise-and #b11111111 num32)))
             (format #f "PORT ~a,~a,~a,~a,~a,~a"
                     byte0 byte1 byte2 byte3
                     (arithmetic-shift srvc-port -8) ; high order byte
                     (bitwise-and #b11111111 srvc-port) ; lower order byte
                     )))))
  

(define (ftp:send-command connection command . maybe-expected)
  (let* ((sock (ftp-connection:command-socket connection))
         (OUT (socket:outport sock))
         (expected (or (safe-first maybe-expected) "2..")))
    (write-string command OUT)
    (write-crlf OUT)
    (ftp:log connection (format #f "<- ~a" command))
    (ftp:read-response connection expected)))


;; This is where we check that the server's 3 digit status code
;; corresponds to what we expected. EXPECTED is a string of the form
;; "250", which indicates we are expecting a 250 code from the server,
;; or "2.." which means that we only require the first digit to be 2
;; and don't care about the rest. If the server's response doesn't
;; match EXPECTED, we raise an ftp:error (which is catchable; look at
;; pop3.scm to see how). Since this is implemented as a regexp, you
;; can also specify more complicated acceptable responses of the form
;; "2[4-6][0-9]". The code permits you to match the server's verbose
;; message too, but beware that the messages change from server to
;; server.
(define (ftp:read-response connection . maybe-expected)
  (let* ((sock (ftp-connection:command-socket connection))
         (IN (socket:inport sock))
         (response (read-line IN))
         (expected (or (safe-first maybe-expected) "2..")))
    (ftp:log connection (format #f "-> ~a" response))
    (or (string-match expected response)
        (signal 'ftp:error response))
    ;; handle multi-line responses
    (if (equal? (string-ref response 3) #\-)
        (let loop ((code (string-append (substring response 0 3) " "))
                   (line (read-line IN)))
          (ftp:log connection (format #f "-> ~a" line))
          (set! response (join-strings (list response line) "\n"))
          (or (string-match code line)
              (loop code (read-line IN)))))
    response))


(define (ftp:build-command-string str opt-args)
  (let ((arg (safe-first opt-args)))
    (if arg
        (join-strings (list str arg))
        str)))

(define (ftp:log connection line)
  (let ((LOG (ftp-connection:logfd connection)))
    (and LOG
         (write-string line LOG)
         (write-string "\n" LOG)
         (force-output LOG))))

;; EOF
