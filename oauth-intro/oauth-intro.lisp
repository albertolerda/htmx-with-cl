;;;; oauth-intro.lisp

(in-package #:oauth-intro)

(defvar *app* (make-instance 'ningle:app))
(defvar *authorize-uri* "http://localhost:9000/application/o/authorize/")
(defvar *token-uri* "http://localhost:9000/application/o/token/")
(defvar *client-id* "...")
(defvar *client-secret* "...")
(defvar *redirect-uri* "http://localhost:5000/oauth-callback")

(defmacro from-session (key)
  `(gethash ,key
	    (getf (lack.request:request-env ningle:*request*)
		  :lack.session)))

(defmacro main-layout (&body body)
  `(spinneret:with-html-string
     (:doctype)
     (:html
      (:head
       (:meta :charset "utf-8")
       (:meta :name "viewport" :content "width=device-width, initial-scale=1")
       (:script :src "https://unpkg.com/htmx.org@1.9.9")
       (:link :rel "stylesheet" :href "https://cdn.jsdelivr.net/npm/bulma@0.9.4/css/bulma.min.css"))
      (:body
       (:section :class "section"
		 ,@body)))))




(defun base64-url-encode (bytes)
  (let* ((base64-str (qbase64:encode-bytes bytes))
         (url-safe-str (substitute #\- #\+ (substitute #\_ #\/ base64-str))))
    (string-right-trim "=" url-safe-str)))


(defun generate-and-save-state ()
  (let ((state (base64-url-encode
		(ironclad:random-data 64))))
    (setf (from-session :oauth-state) state)
    state))

(defun generate-and-save-nonce ()
  (let ((nonce (base64-url-encode
		(ironclad:random-data 64))))
    (setf (from-session :oauth-nonce) nonce)
    nonce))

(defun sha256 (bytes)
  (let ((digester (ironclad:make-digest :sha256)))
    (ironclad:update-digest digester bytes)
    (ironclad:produce-digest digester)))


(defun generate-and-save-challenge ()
  (let ((code-verifier (base64-url-encode (ironclad:random-data 64))))
    (setf (from-session :oauth-code) code-verifier)
    (base64-url-encode
     (sha256 (flexi-streams:string-to-octets code-verifier)))))






(setf (ningle:route *app* "/login")
      #'(lambda (params)
	  (declare (ignore params))	  
	  (let* ((state (generate-and-save-state))
		 (code-challenge (generate-and-save-challenge))
		 (nonce (generate-and-save-nonce))
		 (final-uri (concatenate 'string
					 *authorize-uri* "?"
					 "client_id=" *client-id* "&"
					 "redirect_uri=" *redirect-uri* "&"
					 "state=" state "&"
					 "response_type=code" "&"
					 "code_challenge=" code-challenge "&"
					 "code_challenge_method=S256" "&"
					 "nonce=" nonce "&"
					 )))
	    `(301 (:location ,final-uri)))))


(setf (ningle:route *app* "/oauth-callback")
      #'(lambda (params)
	  (let ((code (cdr (assoc "code" params :test #'string=)))
		(state (cdr (assoc "state" params :test #'string=)))
		(state-req (from-session :oauth-state))
		(code-verifier-req (from-session :oauth-code))
		(nonce-req (from-session :oauth-nonce)))

	    (if (string/= state-req state)
		`(302 (:location "/"))
		(let ((token
			(dex:post *token-uri*
				  :verbose t
				  :headers
				  '(("Content-Type" . "application/x-www-form-urlencoded"))
				  :content
				  `(("client_id" . ,*client-id*)
				    ("client_secret" . ,*client-secret*)
				    ("code" . ,code)
				    ("code_verifier" . ,code-verifier-req)
				    ("grant_type" . "authorization_code")
				    ("redirect_uri" . ,*redirect-uri*)))))
		  `(200 (:content-type "application/json") (,token)))))))


(setf (ningle:route *app* "/")
      #'(lambda (params)
	  (declare (ignore params))
	  (main-layout
	    (:a :href "/login" "Login"))))

(clack:clackup
 (lack.builder:builder
  (:session)
  *app*))
