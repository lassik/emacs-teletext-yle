;;; teletext-yle.el --- Teletext provider for Finnish national network YLE -*- lexical-binding: t -*-
;;
;; SPDX-License-Identifier: ISC
;; Author: Lassi Kortela <lassi@lassi.io>
;; URL: https://github.com/lassik/emacs-teletext-yle
;; Package-Requires: ((emacs "24.3") (cl-lib "0.5") (teletext "0.1"))
;; Package-Version: 0.1.0
;; Keywords: comm help hypermedia
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Provides a teletext broadcast from YLE, the Finnish national
;; television network.  The service is free of charge and a TV tuner
;; is not needed for browsing: an ordinary internet connection is
;; enough.  You don't need a personal YLE account to use this.
;;
;; The broadcast is retrieved from YLE's official public API.  This
;; Emacs package is not made by YLE and is not endorsed by them or
;; affiliated with them in any way.
;;
;; Note: The vast majority of YLE teletext pages are in Finnish with a
;; small selection in Swedish and only news headlines in English.
;;
;;; Code:

(require 'json)

(require 'teletext)

(defvar teletext-yle--auth
  (list "7ae4681c" "effd5dd0d540d1eaade99c1ab466be0a")
  "API key used to download teletext pages from YLE.

The default key is shared with everyone as an act of goodwill.
Please use it only for casual browsing.  If you want to do
experiments that might exceed the rate limit of the API, get
your own free key at <https://developer.yle.fi/>.")

(defun teletext-yle--url (page)
  "Internal helper to get the API URL for an YLE teletext PAGE."
  (concat "https://external.api.yle.fi"
          "/v1/teletext/pages/" (number-to-string page) ".json"
          "?app_id="  (nth 0 teletext-yle--auth)
          "&app_key=" (nth 1 teletext-yle--auth)))

(defun teletext-yle--download-page-json (page)
  "Internal helper to get the JSON for an YLE teletext PAGE."
  (let ((url (teletext-yle--url page)))
    (with-temp-buffer
      (condition-case _
          (let ((url-show-status nil))
            (url-insert-file-contents url)
            (json-read))
        ((file-error)
         (message "Teletext page not found")
         nil)
        ((json-error end-of-file)
         (message "Error decoding teletext page")
         nil)))))

(defun teletext-yle--insert-from-json (parsed-json page subpage)
  "Internal helper to insert the contents of an YLE teletext PAGE.

SUBPAGE is the subpage (1..n) of that page.  PARSED-JSON is an
Emacs Lisp representation of the JSON response corresponding to
PAGE from the YLE API."
  (cl-flet ((vector-to-list (x) (cl-map 'list #'identity x))
            (assoc* (key x) (cdr (assoc key x)))
            (graphics-p (color)
                        (member color '("gblue" "gcyan" "ggreen" "gmagenta"
                                        "gred" "gwhite" "gyellow")))
            (parse-hex (s)
                       (and s (string-match "^[0-9A-F][0-9A-F]h$" s)
                            (cl-parse-integer (substring s 0 2) :radix 16))))
    (let* ((page-json (assoc* 'page (assoc* 'teletext parsed-json)))
           (subpages (vector-to-list (assoc* 'subpage page-json)))
           (this-subpage
            (or (cl-some (lambda (this-subpage)
                           (let* ((snumber (assoc* 'number this-subpage))
                                  (number (and snumber
                                               (string-to-number snumber))))
                             (and (equal number subpage)
                                  this-subpage)))
                         subpages)
                (first subpages))))
      (mapc (lambda (line)
              (let ((runs (assoc* 'run line)))
                (setq runs (if (vectorp runs) runs (vector runs)))
                (dolist (run (vector-to-list runs))
                  (let ((bg (assoc* 'bg run))
                        (fg (assoc* 'fg run))
                        (text (assoc* 'Text run))
                        (charcode (parse-hex (assoc* 'charcode run)))
                        (length (string-to-number (assoc* 'length run))))
                    (setq text (cond ((graphics-p fg)
                                      (make-string length ? ))
                                     (text text)
                                     (t (make-string
                                         length (or charcode ? )))))
                    (when (graphics-p bg) (setq bg (substring bg 1)))
                    (when (graphics-p fg) (setq fg (substring fg 1)))
                    (let ((start (point)))
                      (insert text)
                      (let ((overlay (make-overlay start (point)))
                            (face (teletext-get-face bg fg)))
                        (overlay-put overlay 'face face))))))
              (insert "\n"))
            (vector-to-list
             (assoc* 'line
                     (cl-some (lambda (x)
                                (and (equal "structured" (assoc* 'type x))
                                     x))
                              (vector-to-list
                               (assoc* 'content this-subpage))))))
      (list (cons 'page page)
            (cons 'subpage (if (null this-subpage) 1
                             (string-to-number
                              (assoc* 'number this-subpage))))
            (cons 'subpages (max 1 (length subpages)))
            (cons 'prev-page (let ((pg (assoc* 'prevpg page-json)))
                               (and pg (string-to-number pg))))
            (cons 'next-page (let ((pg (assoc* 'nextpg page-json)))
                               (and pg (string-to-number pg))))
            (cons 'network-heading "YLE TEKSTI-TV")
            (cons 'network-page-text "Sivu:")
            (cons 'network-time-format "{dd}.{mm}. {HH}:{MM}")))))

(defun teletext-yle--page (network page subpage)
  "Internal helper to insert the contents of YLE PAGE/SUBPAGE.

NETWORK must be \"YLE\"."
  (cl-assert (equal network "YLE"))
  (teletext-yle--insert-from-json
   (teletext-yle--download-page-json page) page subpage))

(defun teletext-yle--networks ()
  "Internal helper to get the YLE teletext network list."
  '("YLE"))

(teletext-provide
 'teletext-yle
 :networks #'teletext-yle--networks
 :page #'teletext-yle--page)

(provide 'teletext-yle)

;;; teletext-yle.el ends here
