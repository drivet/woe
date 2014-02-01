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
  (save-current-buffer
    (set-buffer "*BBDB*")
    (json-encode
     (apply 'vector
            (mapcar 'bbdb-record-to-contact
                    (mapcar 'car bbdb-records))))))
 
(defun woe-record-firstname(record)
  (bbdb-record-firstname record))

(defun woe-record-lastname(record)
  (bbdb-record-lastname record))

(defun woe-record-mail(record)
  (apply 'vector (bbdb-record-mail record)))

(defun woe-record-organization(record)
  (apply 'vector (bbdb-record-organization record)))

(defun woe-record-telephone(record)
  (apply 'vector (mapcar 'woe-phone (bbdb-record-phone record))))

(defun woe-record-address(record)
  (apply 'vector (mapcar 'woe-address (bbdb-record-address record))))

(defun woe-phone (bbdb-phone)
  (list ':label (bbdb-phone-label bbdb-phone)
        ':areaCode (bbdb-phone-area bbdb-phone)
        ':prefix (bbdb-phone-exchange bbdb-phone)
        ':suffix (bbdb-phone-suffix bbdb-phone)
        ':extension (bbdb-phone-extension bbdb-phone)))

(defun woe-address (bbdb-address)
  (list ':label (bbdb-address-label bbdb-address)
        ':streetLines (apply 'vector (bbdb-address-streets bbdb-address))
        ':city (bbdb-address-city bbdb-address)
        ':state (bbdb-address-state bbdb-address)
        ':postalCode (bbdb-address-postcode bbdb-address)
        ':country (bbdb-address-country bbdb-address)))

(defun woe-record-hash (record)
  (secure-hash 'md5 (prin1-to-string (list (bbdb-record-firstname record)
                                           (bbdb-record-lastname record)
                                           (bbdb-record-mail record)
                                           (bbdb-record-organization record)
                                           (bbdb-record-phone record)
                                           (bbdb-record-address record)))))

(defun contact-internal (field-list record)
  (if (eq field-list 'nil)
      'nil
    (let ((field-value (funcall (cadar field-list) record)))
      (if (eq field-value 'nil)
          (contact-internal (cdr field-list) record)
        (append (list (caar field-list) field-value)
                (contact-internal (cdr field-list) record))))))
  
(defun bbdb-record-to-contact (record)
  (let ((fields '((:hash woe-record-hash)
                  (:firstName woe-record-firstname)
                  (:lastName woe-record-lastname)
                  (:email woe-record-mail)
                  (:organization woe-record-organization)
                  (:telephone woe-record-telephone)
                  (:address woe-record-address))))
    (contact-internal fields record)))

(defun delete-record-if-match (firstname lastname hash wraprec)
  (let ((record (car wraprec)))
    (if (and (equal firstname (bbdb-record-firstname record))
             (equal lastname (bbdb-record-lastname record))
             (equal hash (woe-record-hash record)))
        (progn
          (bbdb-delete-record-internal record 't)
          't)
      'nil)))

(defun delete-record-from-list (firstname lastname hash records)
  (cond ((eq records 'nil) 'nil)
        ((delete-record-if-match firstname lastname hash (car records)) 't)
        ('t (delete-record-from-list firstname lastname hash (cdr records)))))

(defun woe-delete-record (firstname lastname hash)
  (bbdb (concat firstname " " lastname))
  (save-current-buffer
    (set-buffer "*BBDB*")
    (if (delete-record-from-list firstname lastname hash bbdb-records)
        (save-some-buffers 't)
      'nil)))
