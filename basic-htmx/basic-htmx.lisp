;;;; basic-htmx.lisp

(in-package #:basic-htmx)

(defvar *app* (make-instance 'ningle:app))

(defvar *books* '("Harry potter"))

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

(defun books-component ()
  (spinneret:with-html-string
    (:div :id "film-list"  :class "content"
	  (:ul
	    (dolist (book *books*)
	      (:li book))))))

(setf (ningle:route *app* "/add-book")
      #'(lambda (params)
	  (push (cdr (assoc "txt-book" params :test #'string=)) *books*)
	  (books-component)))

(setf (ningle:route *app* "/")
      #'(lambda (params)
	  (declare (ignore params))
	  (main-layout
	      (:div :class "container"
		    (:form :data-hx-get "add-book"
			   :data-hx-swap "outerHTML"
			   :data-hx-target "#film-list"
			   :|DATA-HX-ON::AFTER-REQUEST| "if(event.detail.successful) this.reset()"
			   (:label :for "txt-book"
				   "Book")
			   (:input :id "txt-book" :name "txt-book" :type "text" :placeholder "Type a book...")
			   (:input :type "submit" :value "Add"))
		    (:raw (books-component))))))

(clack:clackup *app*)
