;;; content-quoter.el --- Quote buffer and file contents  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Paul Nelson

;; Author: Paul D. Nelson <ultrono@gmail.com>
;; Version: 0.1
;; URL: https://github.com/ultronozm/doc-dual-view.el
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides tools for quoting content from buffers and
;; files in various formats (e.g., markdown code blocks, org-mode
;; source blocks).  I'm using it to help work with LLM's that I can
;; access only via a web browser.
;;
;; Main features:
;; - Quote content from visible buffers
;; - Quote content from git-tracked files
;; - Quote content from project files
;; - Reuse previously quoted content via history
;;
;; Basic usage:
;;   M-x content-quoter-visible-buffers-to-clipboard
;;   M-x content-quoter-git-files-to-clipboard
;;   M-x content-quoter-project-files-to-clipboard
;;   M-x content-quoter-from-history-to-clipboard

;;; Code:

(require 'project)

(defun content-quoter-wrap-markdown (content-plist)
  "Wrap CONTENT-PLIST in a markdown code block."
  (format "## %s: %s\n```%s\n%s```\n"
          (plist-get content-plist :type)
          (plist-get content-plist :name)
          (plist-get content-plist :language)
          (plist-get content-plist :content)))

(defun content-quoter-wrap-org (content-plist)
  "Wrap CONTENT-PLIST in an `org-mode' source block."
  (format "* %s: %s\n#+begin_src %s\n%s#+end_src\n"
          (plist-get content-plist :type)
          (plist-get content-plist :name)
          (plist-get content-plist :language)
          (plist-get content-plist :content)))

(defun content-quoter-wrap-xml (content-plist)
  "Wrap CONTENT-PLIST in XML tags following Anthropic's recommendations."
  (format "<document>
  <source type=\"%s\">%s</source>
  <language>%s</language>
  <document_content>
%s  </document_content>
</document>\n"
          (plist-get content-plist :type)
          (plist-get content-plist :name)
          (plist-get content-plist :language)
          (plist-get content-plist :content)))

(defgroup content-quoter nil
  "Quote buffer and file contents in various formats."
  :group 'tools)

(defcustom content-quoter-wrapper #'content-quoter-wrap-markdown
  "Function used to wrap content in appropriate quotation format.
The function should accept three arguments:
- content: The text to be wrapped
- language: String indicating the content's language
- title: String to use as block title/header
It should return the wrapped content as a string."
  :type '(choice (const :tag "Markdown" content-quoter-wrap-markdown)
                 (const :tag "Org Mode" content-quoter-wrap-org)
                 (const :tag "XML Tags" content-quoter-wrap-xml)
                 (function :tag "Custom function"))
  :group 'content-quoter)

(defvar content-quoter-history nil
  "History of content quoter sources.
Each element is a plist with properties:
:sources - list of buffer names or file paths
:directory - working directory when sources were collected
:timestamp - when the sources were collected
:description - human-readable description of the source set")

(defun content-quoter--format-sources-description (sources)
  "Create a descriptive string for SOURCES.
Includes timestamp and abbreviated source names."
  (format "[%s] %s"
          (format-time-string "%Y-%m-%d %H:%M")
          (string-join
           (seq-take-while
            #'identity
            (list
             (string-join (seq-take sources 3) ", ")
             (when (> (length sources) 3)
               (format "and %d more" (- (length sources) 3)))))
           " ")))

(defun content-quoter--make-history-entry (content-plists &optional description)
  "Create a history entry for CONTENT-PLISTS with optional DESCRIPTION."
  (list :content-plists (copy-sequence content-plists)
        :directory default-directory
        :timestamp (current-time)
        :description (or description
                         (content-quoter--format-sources-description
                          (mapcar (lambda (plist)
                                    (format "%s: %s"
                                            (plist-get plist :type)
                                            (plist-get plist :name)))
                                  content-plists)))))

(defun content-quoter-add-to-history (sources &optional description)
  "Add SOURCES to `content-quoter-history' with optional DESCRIPTION."
  (push (content-quoter--make-history-entry sources description)
        content-quoter-history))

(defun content-quoter--move-to-history-top (description)
  "Move the history entry with DESCRIPTION to the top of the history."
  (let ((entry (assoc description
                      (mapcar (lambda (e)
                                (cons (plist-get e :description) e))
                              content-quoter-history))))
    (when entry
      (setq content-quoter-history
            (cons (cdr entry)
                  (delq (cdr entry) content-quoter-history))))))

(defun content-quoter-select-from-history ()
  "Select and return sources from history using completion.
The most recent history item is the default choice.
Selected item becomes the most recent in history."
  (let* ((candidates
          (mapcar (lambda (entry)
                    (cons (plist-get entry :description) entry))
                  content-quoter-history))
         (default (caar candidates))
         (prompt (if default
                     (format "Select previous sources (default %s): " default)
                   "Select previous sources: "))
         (choice (completing-read prompt
                                  candidates
                                  nil t nil nil
                                  default)))
    (content-quoter--move-to-history-top choice)
    (let ((entry (cdr (assoc choice candidates))))
      (when entry
        (let ((default-directory (plist-get entry :directory)))
          (plist-get entry :sources))))))

(defun content-quoter--ensure-trailing-newline (content)
  "Ensure that CONTENT ends with exactly one newline."
  (if (string-match "\n\\'" content)
      content
    (concat content "\n")))

(defun content-quoter--get-mode-for-file (filename)
  "Determine the major mode that would be used for FILENAME."
  (let ((mode (assoc-default filename auto-mode-alist 'string-match)))
    (if (and mode (symbolp mode))
        mode
      'fundamental-mode)))

(defun content-quoter--mode-to-language (mode)
  "Convert MODE name to a language string for syntax highlighting."
  (replace-regexp-in-string "-mode\\'" "" (symbol-name mode)))

(defun content-quoter--get-buffer-content (buffer)
  "Get content and metadata from BUFFER.
Returns a plist with :content, :language, :type, and :name properties."
  (with-current-buffer buffer
    (list :content (content-quoter--ensure-trailing-newline
                    (buffer-substring-no-properties (point-min) (point-max)))
          :language (content-quoter--mode-to-language major-mode)
          :type 'buffer
          :name (buffer-name))))

(defun content-quoter--get-file-content (filepath)
  "Get content and metadata from file at FILEPATH.
Returns a plist with :content, :language, :type, and :name properties."
  (with-temp-buffer
    (insert-file-contents filepath)
    (let ((mode (content-quoter--get-mode-for-file filepath)))
      (list :content (content-quoter--ensure-trailing-newline (buffer-string))
            :language (content-quoter--mode-to-language mode)
            :type 'file
            :name (file-name-nondirectory filepath)))))

(defun content-quoter--combine-sources (sources)
  "Combine multiple quoted SOURCES with a newline between each."
  (string-join sources "\n"))

(defun content-quoter--collect-visible-buffers ()
  "Get list of unique visible buffer names."
  (seq-uniq
   (mapcar #'window-buffer (window-list))))

(defun content-quoter--collect-git-files (wildcard)
  "Get list of git-tracked files matching WILDCARD pattern.
Returns absolute file paths."
  (let ((git-root (locate-dominating-file default-directory ".git")))
    (unless git-root
      (user-error "Not in a git repository"))
    (let* ((git-files
            (split-string
             (shell-command-to-string "git ls-files --full-name")
             "\n" t))
           (matching-files
            (if (or (null wildcard) (string= wildcard "*.*"))
                git-files
              (let ((regexp (wildcard-to-regexp wildcard)))
                (seq-filter (lambda (file)
                              (string-match-p regexp file))
                            git-files)))))
      (mapcar (lambda (file)
                (expand-file-name file git-root))
              matching-files))))

(defun content-quoter--collect-project-files (wildcard)
  "Get list of project files matching WILDCARD pattern.
Returns absolute file paths."
  (let ((pr (project-current)))
    (unless pr
      (user-error "Not in a project"))
    (let* ((all-files
            (let ((project-vc-include-untracked nil))
              (project-files pr)))
           (matching-files
            (if (or (null wildcard) (string= wildcard "*.*"))
                all-files
              (let ((regexp (wildcard-to-regexp wildcard)))
                (seq-filter (lambda (file)
                              (string-match-p regexp file))
                            all-files)))))
      matching-files)))

(defun content-quoter-quote-sources (sources)
  "Convert SOURCES to quoted format.
SOURCES can be buffer objects, buffer names, or file paths."
  (let (content-plists)
    (dolist (source sources)
      (push
       (cond
        ((bufferp source)
         (content-quoter--get-buffer-content source))
        ((and (stringp source) (get-buffer source))  ; Check if it's a buffer name
         (content-quoter--get-buffer-content (get-buffer source)))
        ((and (stringp source) (file-exists-p source))
         (content-quoter--get-file-content source))
        (t
         (message "Warning: Source %s not found" source)
         nil))
       content-plists))
    (content-quoter--combine-sources
     (mapcar content-quoter-wrapper
             (nreverse (delq nil content-plists))))))

;;;###autoload
(defun content-quoter-directory-files-to-clipboard (dir &optional wildcard recursive)
  "Quote content of files from DIR and copy to clipboard.
Optional WILDCARD (e.g., \"*.py\") filters files by pattern.
With prefix arg RECURSIVE, include subdirectories recursively."
  (interactive
   (list (read-directory-name "Select directory: ")
         (read-string "File wildcard (optional, e.g., *.py): " nil nil "*.*")
         current-prefix-arg))
  (let* ((regexp (if (or (null wildcard) (string= wildcard "*.*"))
                     ".*"
                   (wildcard-to-regexp wildcard)))
         (files (if recursive
                    (directory-files-recursively dir regexp recursive)
                  (mapcar (lambda (f) (expand-file-name f dir))
                          (seq-filter (lambda (f)
                                        (and (not (member f '("." "..")))
                                             (not (file-directory-p (expand-file-name f dir)))))
                                      (directory-files dir nil regexp)))))
         (content-plists (mapcar #'content-quoter--get-file-content files))
         (content (content-quoter--combine-sources
                   (mapcar content-quoter-wrapper content-plists))))
    (content-quoter-add-to-history content-plists)
    (kill-new content)
    (message "Copied content from %d file(s) in directory %s%s%s"
             (length files)
             dir
             (if (string= wildcard "*.*")
                 ""
               (format " matching %s" wildcard))
             (if recursive " (including subdirectories)" ""))))

;;;###autoload
(defun content-quoter-visible-buffers-to-clipboard ()
  "Quote content of all visible buffers and copy to clipboard."
  (interactive)
  (let* ((buffers (content-quoter--collect-visible-buffers))
         (content-plists (mapcar #'content-quoter--get-buffer-content buffers))
         (content (content-quoter--combine-sources
                   (mapcar content-quoter-wrapper content-plists))))
    (content-quoter-add-to-history content-plists)
    (kill-new content)
    (message "Copied content from %d visible buffer(s)" (length buffers))))

;;;###autoload
(defun content-quoter-git-files-to-clipboard (&optional wildcard)
  "Quote content of git-tracked files and copy to clipboard.
Optional WILDCARD (e.g., \"*.el\") filters files by pattern."
  (interactive
   (list (read-string "File wildcard (default: *.*): " nil nil "*.*")))
  (let* ((files (content-quoter--collect-git-files wildcard))
         (content-plists (mapcar #'content-quoter--get-file-content files))
         (content (content-quoter--combine-sources
                   (mapcar content-quoter-wrapper content-plists))))
    (content-quoter-add-to-history content-plists)
    (kill-new content)
    (message "Copied content from %d git-tracked file(s)%s"
             (length files)
             (if (string= wildcard "*.*")
                 ""
               (format " matching %s" wildcard)))))

;;;###autoload
(defun content-quoter-project-files-to-clipboard (&optional wildcard)
  "Quote content of project files and copy to clipboard.
Optional WILDCARD (e.g., \"*.el\") filters files by pattern."
  (interactive
   (list (read-string "File wildcard (default: *.*): " nil nil "*.*")))
  (let* ((files (content-quoter--collect-project-files wildcard))
         (content-plists (mapcar #'content-quoter--get-file-content files))
         (content (content-quoter--combine-sources
                   (mapcar content-quoter-wrapper content-plists))))
    (content-quoter-add-to-history content-plists)
    (kill-new content)
    (message "Copied content from %d project file(s)%s"
             (length files)
             (if (string= wildcard "*.*")
                 ""
               (format " matching %s" wildcard)))))

;;;###autoload
(defun content-quoter-from-history-to-clipboard ()
  "Quote content from a previously used set of sources."
  (interactive)
  (if (null content-quoter-history)
      (user-error "No content quoter history available")
    (let* ((sources (content-quoter-select-from-history))
           (content (content-quoter-quote-sources sources)))
      (kill-new content)
      (message "Copied content from historical source set"))))

;;;###autoload
(defun content-quoter-selected-buffers-to-clipboard ()
  "Quote content of selected buffers and copy to clipboard.
Uses `completing-read-multiple' to select buffers from all available buffers."
  (interactive)
  (let* ((selected-buffers
          (mapcar #'get-buffer
                  (completing-read-multiple
                   "Select buffers to quote: "
                   (mapcar #'buffer-name (buffer-list))
                   nil t)))
         (content-plists (mapcar #'content-quoter--get-buffer-content selected-buffers))
         (content (content-quoter--combine-sources
                   (mapcar content-quoter-wrapper content-plists))))
    (content-quoter-add-to-history content-plists)
    (kill-new content)
    (message "Copied content from %d selected buffer(s)" (length selected-buffers))))

(provide 'content-quoter)
;;; content-quoter.el ends here
