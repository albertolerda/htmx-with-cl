;;;; htmx-examples.lisp

(in-package #:htmx-examples-pt2)

(eval-when
    (:compile-toplevel
     :load-toplevel
     :execute)
  (setf spinneret:*unvalidated-attribute-prefixes* '("")))

(defvar *contacts* '((:first "Joe" :last "Blow" :email "jow@blow.com" :active t)
		     (:first "Angie" :last "MacDowell" :email "angie@macdowell.org" :active nil)))

(defvar *app* (make-instance 'ningle:app))

(defun remove-nth (list pos)
  (if (and list (> pos 0))
      (cons (car list) (remove-nth (cdr list) (1- pos)))
      (cdr list)))

(defun delete-contact (i)
  (setf *contacts* (remove-nth *contacts* i)))

(defmacro main-layout (&body body)
  `(spinneret:with-html-string
     (:doctype)
     (:html
      (:head
       (:meta :charset "utf-8")
       (:meta :name "viewport" :content "width=device-width, initial-scale=1")
       (:script :src "https://unpkg.com/hyperscript.org@0.9.12")
       (:script :src "https://unpkg.com/htmx.org@1.9.9")
       (:link  :rel "stylesheet" :href "https://cdn.jsdelivr.net/npm/bootstrap@3.4.1/dist/css/bootstrap.min.css" :integrity "sha384-HSMxcRTRxnN+Bdg0JdbxYKrThecOKuH5zCYotlSAcp1+c8xmyTe9GYg1l9a69psu" :crossorigin "anonymous")
       (:style "tr.htmx-swapping td {
  opacity: 0;
  transition: opacity 1s ease-out;
}"))
      (:body
       (:section :class "container"
		 (:div :class "row"
		       ,@body))))))

(defconstant +N_PAGE_ITEMS+ 10)

(defun agent-page (n-page)
  (spinneret:with-html-string
    (loop for i from (* n-page +N_PAGE_ITEMS+) to (1- (* (1+ n-page) +N_PAGE_ITEMS+))
	  do (:tr
	      (:td "Agent Smith")
	      (:td (format nil "agentsmith~a@null.org" i))
	      (:td i)))))

(defun agents-table (n-page)
  (spinneret:with-html-string
    (:raw (agent-page n-page))
    (:tr :id "replaceMe"
	 (:td :colspan "3"
	      (:button :class "btn"
		       :data-hx-get (format nil "/agents?page=~a" (1+ n-page))
		       :data-hx-target "#replaceMe"
		       :data-hx-swap "outerHTML" "Load More Agents..."
		       (:img :class "htmx-indicator" :src "/img/bars.svg"))))))

(setf (ningle:route *app* "/agents")
      #'(lambda (params)
	  (let ((n-page (cdr (assoc "page" params :test #'equalp))))
	    (if n-page
		(agents-table (parse-integer n-page))
		(main-layout
		  (:table :class "table"
			  (:raw (agents-table 0))))))))

(defun contact-component (i)
  (let ((contact (elt *contacts* i)))
    (spinneret:with-html-string
      (:tr
       (:td (concatenate 'string (getf contact :first)
			 " " (getf contact :last)))
       (:td (getf contact :email))
       (:td (if (getf contact :active)
		"Active" "Inactive"))
       
       
       (:td
	(:button :class "btn btn-danger"
		 :data-hx-get
		 (format nil "/contacts/~a/edit" i)
		 :data-hx-trigger "edit"
		 :_ "on click
                     if .editing is not empty
                       Swal.fire({title: 'Already Editing',
                                  showCancelButton: true,
                                  confirmButtonText: 'Yep, Edit This Row!',
                                  text:'Hey!  You are already editing a row!  Do you want to cancel that edit and continue?'})
                       if the result's isConfirmed is false
                         halt
                       end
                       send cancel to .editing
                     end
                     trigger edit" "Edit")
	(:button :class "btn btn-danger"
		 :data-hx-delete (format nil "/contacts/~a" i)
		 "Delete"))))))

(setf (ningle:route *app* "/contacts")
      #'(lambda (params)
	  (declare (ignore params))
	  (main-layout
	    (:table :class "table delete-row-example"
		    (:thead
		     (:tr
		      (:th "Name")
		      (:th "Email")
		      (:th "Status")
		      (:th)))
		    (:tbody :data-hx-confirm "Are you sure?"
			    :data-hx-target "closest tr"
			    :data-hx-swap "outerHTML swap:1s"
			    (loop for contact in *contacts* for i from 0 do
			      (:raw (contact-component i))))))))

(setf (ningle:route *app* "/contacts/:id")
      #'(lambda (params)
	  (let ((id (parse-integer (cdr (assoc :id params)))))
	    (contact-component id))))

(setf (ningle:route *app* "/contacts/:id" :method :PUT)
      #'(lambda (params)
	  (let* ((id (parse-integer (cdr (assoc :id params))))
		 (first (cdr (assoc "firstName" params :test #'equalp)))
		 (last (cdr (assoc "lastName" params :test #'equalp)))
		 (email (cdr (assoc "email" params :test #'equalp)))
		 (contact (elt *contacts* id)))
	    (when first (setf (getf contact :first) first))
	    (when last (setf (getf contact :last) last))
	    (when email (setf (getf contact :email) email))
	    (contact-component id))))

(setf (ningle:route *app* "/contacts/:id/edit")
      #'(lambda (params)
	  (let* ((id (parse-integer (cdr (assoc :id params))))
		 (contact (elt *contacts* id)))
	    (spinneret:with-html-string
	      (:tr :data-hx-trigger "cancel" :class "editing"
		   :data-hx-get (format nil "/contacts/~a" id)
		   (:td
		    (:input :name "firstName" :value (getf contact :first)))
		   (:td
		    (:input :name "email" :value (getf contact :email)))
		   (:td
		    (:button :class "btn btn-danger"
			     :hx-get (format nil "/contacts/~a" id) "Cancel")
		    (:button :class "btn btn-danger"
			     :hx-put (format nil "/contacts/~a" id)
			     :hx-include "closest tr" "Save")))))))

(setf (ningle:route *app* "/contacts/:id" :method :DELETE)
      #'(lambda (params)
	  (let ((id (parse-integer (cdr (assoc :id params)))))
	    (delete-contact id)
	    "")))

(clack:clackup *app*)
