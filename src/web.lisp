(in-package :cl-user)
(defpackage cl-warehouse.web
  (:use :cl
        :caveman2
        :cl-warehouse.config
        :cl-warehouse.view
        :cl-warehouse.db
        :datafly
        :sxql)
  (:export :*web*))
(in-package :cl-warehouse.web)

;; for @route annotation
(syntax:use-syntax :annot)

;;
;; Application

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)

;; Db connection
(mito:connect-toplevel :sqlite3 :database-name (merge-pathnames #P"warehouse.db" *application-root*))

(mito:deftable user ()
  ((name :col-type (:varchar 64))
   (email :col-type (or (:varchar 128) :null))))

(mito:ensure-table-exists 'user)

;; (defvar me
;;   (make-instance 'user :name "Eitaro Fukamachi" :email "e.arrows@gmail.com"))

;; (mito:insert-dao me)

;;
;; Routing rules

(defroute "/" ()
  (print (mito:find-dao 'user))
  (render #P"index.html"))

(defroute "/warehouses" ()
  (render #P"warehouses.html"))

(defroute "/boxes" ()
  (render #P"boxes.html"))
;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
