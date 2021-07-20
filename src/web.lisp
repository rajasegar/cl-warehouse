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

;; (mito:deftable user ()
;;   ((name :col-type (:varchar 64))
;;    (email :col-type (or (:varchar 128) :null))))

(mito:deftable warehouses ()
  ((location :col-type (:varchar 50))
   (capacity :col-type (:integer))))

(mito:deftable boxes ()
  ((contents :col-type (:varchar 10))
   (value :col-type (:integer))
   (warehouse :col-type warehouses :references warehouses)))

;; (mito:ensure-table-exists 'user)
(mito:ensure-table-exists 'warehouses)
(mito:ensure-table-exists 'boxes)


;; (defvar me
;;   (make-instance 'user :name "Eitaro Fukamachi" :email "e.arrows@gmail.com"))

;; (mito:insert-dao me)

;; (mito:insert-dao (make-instance 'warehouses :location "Chicago" :capacity 3))
;; (mito:insert-dao (make-instance 'warehouses :location "Chicago" :capacity 4))
;; (mito:insert-dao (make-instance 'warehouses :location "New York" :capacity 7))
;; (mito:insert-dao (make-instance 'warehouses :location "Los Angeles" :capacity 2))
;; (mito:insert-dao (make-instance 'warehouses :location "San Francisco" :capacity 8))

;; (mito:insert-dao (make-instance 'boxes :contents "Rocks" :value 180 :warehouse-id 1))
;; (mito:insert-dao (make-instance 'boxes :contents "Paper" :value 250 :warehouse-id 1))
;; (mito:insert-dao (make-instance 'boxes :contents "Scissors" :value 90 :warehouse-id 1))
;; (mito:insert-dao (make-instance 'boxes :contents "Rocks" :value 180 :warehouse-id 2))
;; (mito:insert-dao (make-instance 'boxes :contents "Paper" :value 250 :warehouse-id 2))
;; (mito:insert-dao (make-instance 'boxes :contents "Scissors" :value 90 :warehouse-id 2))
;; (mito:insert-dao (make-instance 'boxes :contents "Rocks" :value 180 :warehouse-id 3))
;; (mito:insert-dao (make-instance 'boxes :contents "Paper" :value 250 :warehouse-id 3))
;; (mito:insert-dao (make-instance 'boxes :contents "Scissors" :value 90 :warehouse-id 3))
;; (mito:insert-dao (make-instance 'boxes :contents "Rocks" :value 180 :warehouse-id 4))
;; (mito:insert-dao (make-instance 'boxes :contents "Paper" :value 250 :warehouse-id 5))
;;
;; Routing rules

(defroute "/" ()
  (render #P"index.html"))

(defroute "/warehouses" ()
  (render #P"warehouses.html" (list :warehouses (mito:retrieve-dao 'warehouses))))

(defroute "/boxes" ()
  (render #P"boxes.html" (list :boxes (mito:retrieve-dao 'boxes))))
;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
