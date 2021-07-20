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

(mito:deftable warehouses ()
  ((location :col-type (:varchar 50))
   (capacity :col-type (:integer))))

(mito:deftable boxes ()
  ((contents :col-type (:varchar 10))
   (value :col-type (:integer))
   (warehouse :col-type warehouses :references warehouses)))

(mito:ensure-table-exists 'warehouses)
(mito:ensure-table-exists 'boxes)

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

;; utils
(defun get-param (name parsed)
  "Get param values from _parsed"
  (cdr (assoc name parsed :test #'string=)))
;;
;; Routing rules

(defroute "/" ()
  (render #P"index.html" (list :active "/")))

(defroute "/warehouses" ()
  (render #P"warehouses/index.html" (list :warehouses (mito:retrieve-dao 'warehouses)
					  :active "/warehouses")))

(defroute ("/warehouses" :method :POST) (&key _parsed)
  (print _parsed)
  (let ((new-warehouse (make-instance 'warehouses
                                      :location (get-param "location" _parsed)
                                      :capacity (get-param "capacity" _parsed))))
    (mito:insert-dao new-warehouse)
    (redirect "/warehouses")))

(defroute "/warehouses/new" ()
  (render #P"warehouses/new.html"))

(defroute "/warehouses/:id/edit" (&key id)
  (let ((warehouse (mito:find-dao 'warehouses :id id)))
  (render #P"warehouses/edit.html" (list :warehouse warehouse))))

(defroute ("/warehouses/:id/update" :method :POST) (&key id _parsed)
  (let ((warehouse (mito:find-dao 'warehouses :id id)))
    (setf (slot-value warehouse 'location) (get-param "location" _parsed)
          (slot-value warehouse 'capacity) (get-param "capacity" _parsed))
    (mito:save-dao warehouse)
    (redirect "/warehouses")))

(defroute "/warehouses/:id/delete" (&key id)
  (mito:delete-by-values 'warehouses :id id)
  (redirect "/warehouses"))

(defroute "/warehouses/:id" (&key id)
  (let ((warehouse (mito:find-dao 'warehouses :id id)))
  (render #P"warehouses/show.html" (list :warehouse warehouse))))

(defroute "/boxes" ()
  (render #P"boxes/index.html" (list
				:boxes (mito:select-dao 'boxes (mito:includes 'warehouses))

				:active "/boxes")))

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
