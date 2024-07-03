#lang deinprogramm/sdp

;; DATUM TYPES

(define Datum
  (signature
   (mixed DI DS DR DL)))

(define-record DI
  make-DI
  DI?
  (DI-int integer))

(define-record DS
  make-DS
  DS?
  (DS-str string))

;; named record with fixed fields
(define-record DR
  make-DR
  DR?
  (DR-datums (list-of Datum)))

;; list of same datums (e.g. list of DI)
(define-record DL
  make-DL
  DL?
  (DL-datums (list-of Datum)))

;; TODO: maybe implement:
;; - Bool, Float
;; - DA (? (named) alternative; super-type name, record name, fields) see:
;;   https://gitlab.active-group.de/ag/TImporter/-/blob/master/Tim/src/haskell/Tim.hs


;; SPREADSHEET FORMAT

; cell type
(define CellType (signature (enum "Int" "String")))

(define Format
  (signature
   (mixed Cell Text Record)))

;; single cell with string parser to Type
(define-record Cell
  make-cell
  cell?
  (cell-type CellType)
  (cell-string-parser (string -> Datum))) 

;; single cell with fixed string text
(define-record Text
  make-text
  text?
  (text-value string))

;; Coordinates,
;;  TODO: what is: Labeled Label -- labeled coordinates (absolute via LabeledList) ?
;;     see: https://gitlab.active-group.de/ag/TImporter/-/blob/master/Tim/src/haskell/Tim.hs#L56
(define-record Coordinates
  make-coordinates
  coordinates?
  (coordinate-row integer)
  (coordinate-col integer))

(define-record RecordEntry
  make-entry
  entry?
  (entry-coordinates Coordinates)
  (entry-format Format))

(define-record Record
  make-record
  record?
  (record-name string)
  (record-entries (list-of RecordEntry)))

(define-record vector
  make-vector
  vector?
  (vector-1 integer)
  (vector-2 integer))

(define up (make-vector -1 0))
(define down (make-vector 1 0))
(define left (make-vector 0 -1))
(define right (make-vector 0 1))

(define Direction (signature (enum up down left right)))

(define-record List
  make-list
  list?
  (list-direction Direction)
  (list-format Format))
;; TODO: maybe implement:
;; - Alternatives JavaName [(String, JavaName, [(Format, JavaName, Coordinates)])]
;; - LabeledList Int Direction [Coordinates] [Label] Format
;; - DRecord 
