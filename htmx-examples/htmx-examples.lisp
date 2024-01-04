;;;; htmx-examples.lisp

(in-package #:htmx-examples)

(defvar *contacts* '((:first "Joe" :last "Blow" :email "jow@blow.com" :active t)
		     (:first "Angie" :last "MacDowell" :email "angie@macdowell.org" :active nil)))

(defvar *app* (make-instance 'ningle:app))

(defmacro main-layout (&body body)
  `(spinneret:with-html-string
     (:doctype)
     (:html
      (:head
       (:meta :charset "utf-8")
       (:meta :name "viewport" :content "width=device-width, initial-scale=1")
       (:script :src "https://unpkg.com/htmx.org@1.9.9")
       (:link  :rel "stylesheet" :href "https://cdn.jsdelivr.net/npm/bootstrap@3.4.1/dist/css/bootstrap.min.css" :integrity "sha384-HSMxcRTRxnN+Bdg0JdbxYKrThecOKuH5zCYotlSAcp1+c8xmyTe9GYg1l9a69psu" :crossorigin "anonymous"))
      (:body
       (:section :class "container"
		 (:div :class "row"
		       ,@body))))))

(defun contact-component (id)
  (let ((contact (elt *contacts* id)))
    (spinneret:with-html-string
      (:div :data-hx-target "this"
	    :data-hx-swap "outerHTML"
	    (:div
	     (:label "First Name") ":" (getf contact :first))
	    (:div
	     (:label "Last Name") ":" (getf contact :last))
	    (:div
	     (:label "Email") ":" (getf contact :email))
	    (:button :data-hx-get (format nil "/contacts/~a/edit" id)
		     :class "btn btn-primary" "Click To Edit")))))

(setf (ningle:route *app* "/contacts/:id")
      #'(lambda (params)
	  (let ((id (parse-integer (cdr (assoc :id params)))))
	    (main-layout
	      (:raw (contact-component id))))))

(setf (ningle:route *app* "/contacts/:id" :method :PUT)
      #'(lambda (params)
	  (let* ((id (parse-integer (cdr (assoc :id params))))
		 (first (cdr (assoc "firstName" params :test #'equalp)))
		 (last (cdr (assoc "lastName" params :test #'equalp)))
		 (email (cdr (assoc "email" params :test #'equalp)))
		 (contact (elt *contacts* id)))
	    (setf (getf contact :first) first)
	    (setf (getf contact :last) last)
	    (setf (getf contact :email) email)
	    (contact-component id))))

(setf (ningle:route *app* "/contacts/:id/edit")
      #'(lambda (params)
	  (let* ((id (parse-integer (cdr (assoc :id params))))
		 (contact (elt *contacts* id))
		 (contact-url (format nil "/contacts/~a" id)))
	    (spinneret:with-html-string
	      (:form :data-hx-put contact-url
		     :data-hx-target "this"
		     :data-hx-swap "outerHTML"
		     (:div
		      (:label "First Name")
		      (:input :type "text" :name "firstName" :value (getf contact :first)))
		     (:div :class "form-group"
			   (:label "Last Name")
			   (:input :type "text" :name "lastName" :value (getf contact :last)))
		     (:div :class "form-group"
			   (:label "Email Address")
			   (:input :type "email" :name "email" :value (getf contact :email)))
		     (:button :class "btn" "Submit")
		     (:button :class "btn" :data-hx-get contact-url "Cancel"))))))

(defun contacts-table-component ()
  (spinneret:with-html-string
    (loop for contact in *contacts* for i from 0 do
			      (:tr :class ""
				   (:td
				    (:input :type "checkbox" :name "ids" :value i))
				   (:td (concatenate 'string (getf contact :first)
						     " " (getf contact :last)))
				   (:td (getf contact :email))
				   (:td (if (getf contact :active)
					    "Active" "Inactive"))))))

(setf (ningle:route *app* "/contacts")
      #'(lambda (params)
	  (declare (ignore params))
	  (main-layout
	    (:div :data-hx-include "#checked-contacts" :data-hx-target "#tbody"
		  (:button :class "btn" :data-hx-put "/activate" "Activate")
		  (:button :class "btn" :data-hx-put "/deactivate" "Deactivate"))
	    (:form :id "checked-contacts"
		   (:table
		    (:thead
		     (:tr
		      (:th)
		      (:th "Name")
		      (:th "Email")
		      (:th "Status")))
		    (:tbody :id "tbody"
			    (:raw (contacts-table-component))))))))

(setf (ningle:route *app* "/activate" :method :PUT)
      #'(lambda (params)
	  (loop for (name . val) in params do
	    (setf (getf (elt *contacts* (parse-integer val)) :active) t))
	  (contacts-table-component)))

(setf (ningle:route *app* "/deactivate" :method :PUT)
      #'(lambda (params)
	  (loop for (name . val) in params do
	    (setf (getf (elt *contacts* (parse-integer val)) :active) nil))
	  (contacts-table-component)))

(clack:clackup *app*)
