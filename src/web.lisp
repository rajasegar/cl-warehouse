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

(defun get-order-by (direction sort-by)
  "Construct sxql:order-by list values"
  (let ((dir (if (string= direction "asc")
                 :asc
                 :desc))
        (sortby (cond
                  ((string= sort-by "location") :location)
                  ((string= sort-by "capacity") :capacity)
                  ((string= sort-by "contents") :contents)
                  ((string= sort-by "value") :value)
                  (t :id))))
    (list dir sortby)))

;;
;; Routing rules

(defroute "/" ()
  (render #P"index.html" (list :active "/")))

;; GET /warehouses
;; List of warehouses
(defroute "/warehouses" (&key _parsed)
  (let* ((direction (or (get-param "direction" _parsed) "asc"))
        (sort-by (or (get-param "sort-by" _parsed) "location"))
         (warehouses (mito:select-dao 'warehouses
                       (sxql:order-by (get-order-by direction sort-by)))))
    (render #P"warehouses/index.html"
            (list
             :warehouses warehouses
             :active "/warehouses"
             :direction direction
             :sort-by sort-by))))

;; POST /warehouses
;; Create a new warehouse
(defroute ("/warehouses" :method :POST) (&key _parsed)
  (print _parsed)
  (let ((new-warehouse (make-instance 'warehouses
                                      :location (get-param "location" _parsed)
                                      :capacity (get-param "capacity" _parsed))))
    (mito:insert-dao new-warehouse)
    (redirect "/warehouses")))

;; GET /warehouses/new
;; New warehouse page
(defroute "/warehouses/new" ()
  (render #P"warehouses/new.html"))

;; GET /warehouses/:id/edit
;; Edit warehouse
(defroute "/warehouses/:id/edit" (&key id)
  (let ((warehouse (mito:find-dao 'warehouses :id id)))
  (render #P"warehouses/edit.html" (list :warehouse warehouse))))

;; POST /warehouses/:id/update
;; Update warehouse
(defroute ("/warehouses/:id/update" :method :POST) (&key id _parsed)
  (let ((warehouse (mito:find-dao 'warehouses :id id)))
    (setf (slot-value warehouse 'location) (get-param "location" _parsed)
          (slot-value warehouse 'capacity) (get-param "capacity" _parsed))
    (mito:save-dao warehouse)
    (redirect "/warehouses")))

;; GET /warehouses/:id/delete
;; Delete warehouse
(defroute "/warehouses/:id/delete" (&key id)
  (mito:delete-by-values 'warehouses :id id)
  (redirect "/warehouses"))

;; GET /warehouses/:id
;; Warehouse detail page
(defroute "/warehouses/:id" (&key id)
  (let ((warehouse (mito:find-dao 'warehouses :id id)))
  (render #P"warehouses/show.html" (list :warehouse warehouse))))

;; GET /boxes
;; Boxes list page
(defroute "/boxes" (&key _parsed)
  (let* ((direction (or (get-param "direction" _parsed) "asc"))
         (sort-by (or (get-param "sort-by" _parsed) "contents"))
         (boxes (mito:select-dao 'boxes (mito:includes 'warehouses)
                  (sxql:order-by (get-order-by direction sort-by)))))
  (render #P"boxes/index.html" (list
				:boxes boxes
				:active "/boxes"
        :direction direction
        :sort-by sort-by))))

;; GET /boxes/new
;; New box page
(defroute "/boxes/new" ()
  (render #P"boxes/new.html" (list :warehouses (mito:select-dao 'warehouses))))

;; POST /boxes
;; Create a new box
(defroute ("/boxes" :method :POST) (&key _parsed)
  (let ((new-box (make-instance 'boxes
                                      :contents (get-param "contents" _parsed)
                                      :value (get-param "value" _parsed)
                                      :warehouse (mito:find-dao 'warehouses :id (get-param "warehouse" _parsed)))))
    (mito:insert-dao new-box)
    (redirect "/boxes")))
;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
