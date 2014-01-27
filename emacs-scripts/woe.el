(require 'package)
(package-initialize)
(add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("marmalade" .  "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(require 'bbdb)
(require 'bbdb-com)
(require 'json)
(require 'cl)

(defun dump-buffer (buffer-name)
  "Dump contents of buffer to standard out in batch mode"
  (let ((buf (get-buffer buffer-name)))
    (if buf
        (save-current-buffer
          (set-buffer buf)
          (print (buffer-string))))))

(defun fetch-contacts-json-batch (text)
  (princ (fetch-contacts-json text)))
  
(defun fetch-contacts-json (text)
  (bbdb text)
  (set-buffer "*BBDB*")
  (json-encode
   (apply 'vector
          (mapcar 'bbdb-record-to-contact-plist
                  (mapcar 'car bbdb-records)))))
 
(defun woe-record-firstname(record)
  (bbdb-record-firstname record))

(defun woe-record-lastname(record)
  (bbdb-record-lastname record))

(defun woe-record-mail(record)
  (apply 'vector (bbdb-record-mail record)))

(defun woe-record-organization(record)
  (apply 'vector (bbdb-record-organization record)))

(defun woe-record-telephone(record)
  (apply 'vector (mapcar 'woe-phone-plist (bbdb-record-phone record))))

(defun woe-record-address(record)
  (apply 'vector (mapcar 'woe-address-plist (bbdb-record-address record))))

(defun woe-phone-plist (bbdb-phone)
  (list ':label (bbdb-phone-label bbdb-phone)
        ':areaCode (bbdb-phone-area bbdb-phone)
        ':prefix (bbdb-phone-exchange bbdb-phone)
        ':suffix (bbdb-phone-suffix bbdb-phone)
        ':extension (bbdb-phone-extension bbdb-phone)))

(defun woe-address-plist (bbdb-address)
  (list ':label (bbdb-address-label bbdb-address)
        ':streetLines (apply 'vector (bbdb-address-streets bbdb-address))
        ':city (bbdb-address-city bbdb-address)
        ':state (bbdb-address-state bbdb-address)
        ':postalCode (bbdb-address-postcode bbdb-address)
        ':country (bbdb-address-country bbdb-address)))

(defun contact-internal (field-list record)
  (if (eq field-list 'nil)
      'nil
    (let ((field-value (funcall (cadar field-list) record)))
      (if (eq field-value 'nil)
          (contact-internal (cdr field-list) record)
        (append (list (caar field-list) field-value)
                (contact-internal (cdr field-list) record))))))
  
(defun bbdb-record-to-contact-plist (record)
  (let ((fields '((:firstName woe-record-firstname)
                  (:lastName woe-record-lastname)
                  (:email woe-record-mail)
                  (:organization woe-record-organization)
                  (:telephone woe-record-telephone)
                  (:address woe-record-address))))
    (contact-internal fields record)))
