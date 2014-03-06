(require 'package)
(package-initialize)
(add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("marmalade" .  "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(require 'bbdb)
(require 'bbdb-com)
(require 'json)
(require 'cl)

(defun woe-json-read-from-string (value)
  (let ((json-object-type 'plist))
    (json-read-from-string value)))

(defun to-list(vec)
  "convert a vector to a list"
  (append vec 'nil))

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
  (apply 'vector (mapcar 'bbdb-phone-to-woe (bbdb-record-phone record))))

(defun woe-record-address(record)
  (apply 'vector (mapcar 'bbdb-address-to-woe (bbdb-record-address record))))

(defun bbdb-phone-to-woe (bbdb-phone)
  "Converts a bbdb phone vector into a JSON friendly plist"
  (list ':label (bbdb-phone-label bbdb-phone)
        ':areaCode (bbdb-phone-area bbdb-phone)
        ':prefix (bbdb-phone-exchange bbdb-phone)
        ':suffix (bbdb-phone-suffix bbdb-phone)
        ':extension (bbdb-phone-extension bbdb-phone)))

(defun bbdb-address-to-woe (bbdb-address)
  "Converts a bbdb address vector into a JSON friendly plist"
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
  "convert a bbdb record into the list of fields given"
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

(defun woe-phones-to-bbdb-phone-vectors(phones)
  (mapcar 'woe-phone-to-bbdb phones))

(defun woe-addresses-to-bbdb-address-vectors(addresses)
  (mapcar 'woe-address-to-bbdb addresses))

(defun woe-delete-record (firstname lastname hash)
  "Delete record from BBDB, using the first and last names, plus the hash
that was given when you fetched the record."
  (bbdb (concat firstname " " lastname))
  (save-current-buffer
    (set-buffer "*BBDB*")
    (if (delete-record-from-list firstname lastname hash bbdb-records)
        (save-some-buffers 't)
      'nil)))

(defun val-to-num (val)
  (if (equal val 'nil)
      0
    (string-to-number val)))
             
(defun woe-phone-to-bbdb (woe-phone)
  "Converts a plist (JSON-friendly) phone into a bbdb phone vector"
  (let ((phone (make-vector 5 nil)))
    (bbdb-phone-set-label phone (plist-get woe-phone ':label))
    (bbdb-phone-set-area phone (val-to-num (plist-get woe-phone ':areaCode)))
    (bbdb-phone-set-exchange phone (val-to-num (plist-get woe-phone ':prefix)))
    (bbdb-phone-set-suffix phone (val-to-num (plist-get woe-phone ':suffix)))
    (bbdb-phone-set-extension phone (val-to-num (plist-get woe-phone ':extension)))
    phone))

(defun woe-address-to-bbdb (woe-address)
  "Converts a plist (JSON-friendly) address into a bbdb address vector"
  (let ((address (vector "" () "" "" "" "")))
    (bbdb-address-set-label address (plist-get woe-address ':label))
    (bbdb-address-set-streets address (to-list (plist-get woe-address ':streetLines)))
    (bbdb-address-set-city address (plist-get woe-address ':city))
    (bbdb-address-set-state address (plist-get woe-address ':state))
    (bbdb-address-set-postcode address (plist-get woe-address ':postalCode))
    (bbdb-address-set-country address (plist-get woe-address ':country))
    address))

(defun update-bbdb-firstname (record firstname)
  (bbdb-record-set-field record 'firstname fistname))

(defun update-bbdb-lastname (record lastname)
  (bbdb-record-set-field record 'lastname lastname))

(defun update-bbdb-emails (record emails)
  (bbdb-record-set-field record 'mail (to-list emails)))

(defun update-bbdb-organizations (record organizations)
  (bbdb-record-set-field record 'organization (to-list organizations)))

(defun update-bbdb-phones (record phones)
  (bbdb-record-set-field
   record 'phone (woe-phones-to-bbdb-phone-vectors phones) 'nil 't))

(defun update-bbdb-addresses (record addresses)
  (bbdb-record-set-field
   record 'address (woe-addresses-to-bbdb-address-vectors addresses) 'nil 't))
  
(defun update-record (contact-fields record)
  "contact-fields is a property list of fields and values from the JSON"
  (if (equal contact-fields 'nil) 'nil
    (let ((field (car contact-fields))
          (value (cadr contact-fields)))
      (cond ((equal field ':firstName) (update-bbdb-firstname record value))
            ((equal field ':lastName) (update-bbdb-lastname record value))
            ((equal field ':email) (update-bbdb-emails record value))
            ((equal field ':organization) (update-bbdb-organizations record value))
            ((equal field ':telephone) (update-bbdb-phones record value))
            ((equal field ':address) (update-bbdb-addresses record value)))
      (update-record (cddr contact-fields) record))))
                      
(defun update-record-if-match (firstname lastname hash contact wraprec)
  "return non-nil if there is a match"
  (let ((record (car wraprec)))
    (if (and (equal firstname (bbdb-record-firstname record))
             (equal lastname (bbdb-record-lastname record))
             (equal hash (woe-record-hash record)))
        (progn
          (update-record contact record)
          (bbdb-change-record record)
          't)
      'nil)))

(defun update-record-from-list (firstname lastname hash contact records)
  (cond ((eq records 'nil) 'nil)
        ((update-record-if-match firstname lastname hash contact (car records)) 't)
        ('t (update-record-from-list firstname lastname hash contact (cdr records)))))
  
(defun woe-update-record (firstname lastname hash contact)
  "Update a BBDB record using the first name last names, plus 
the hash that was given when we fetched the record. The contact 
argument is the json contact in elisp structured format"
  (bbdb (concat firstname " " lastname))
  (save-current-buffer
    (set-buffer "*BBDB*")
    (if (update-record-from-list firstname lastname hash contact bbdb-records)
        (save-some-buffers 't)
      'nil)))

(defun woe-update-record-json  (firstname lastname hash contact-json)
   "Update a BBDB record using the first name last names, plus 
the hash that was given when we fetched the record. The contact 
argument is the json contact, as a json string"
   (message "converted json %s" (woe-json-read-from-string contact-json))
   (woe-update-record firstname
                      lastname
                      hash
                      (woe-json-read-from-string contact-json)))
