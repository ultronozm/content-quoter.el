;;; content-quoter.el --- Quote buffer and file contents  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Paul Nelson

;; Author: Paul D. Nelson <ultrono@gmail.com>
;; Version: 0.3
;; URL: https://github.com/ultronozm/content-quoter.el
;; Package-Requires: ((emacs "28.1"))
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
;; source blocks).  It's useful for preparing content for external
;; tools accessed via web browsers.
;;
;; Main features:
;; - DWIM command for common quoting tasks
;; - Quote content from current buffer, visible buffers, selected buffers
;; - Quote content from git-tracked files, project files, directory files
;; - Quote content from marked files in Dired
;; - Apply formatting (Markdown, Org, XML, custom)
;; - Reuse previously quoted content sets via history
;;
;; Basic usage:
;;   M-x content-quoter-dwim
;;   M-x content-quoter-visible-buffers-to-clipboard
;;   M-x content-quoter-git-files-to-clipboard
;;   M-x content-quoter-project-files-to-clipboard
;;   M-x content-quoter-from-history-to-clipboard

;;; Code:

(require 'project)
(require 'dired)

;;; Core Data Structure & Formatting Wrappers

;; The core internal data structure is a LIST of PLISTS.
;; Each PLIST represents a single source (buffer or file) and has the keys:
;; :content (string) - The actual text content.
;; :language (string) - The language identifier (e.g., "emacs-lisp").
;; :type (symbol) - The type of source, e.g., 'buffer, 'file, 'region.
;; :name (string) - The name of the source (buffer name, filename, etc.).

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
  "Function used to wrap content.
The function should accept one argument: a content-plist
containing :content, :language, :type, and :name.
It should return the wrapped content as a string."
  :type '(choice (const :tag "Markdown" content-quoter-wrap-markdown)
                 (const :tag "Org Mode" content-quoter-wrap-org)
                 (const :tag "XML Tags" content-quoter-wrap-xml)
                 (function :tag "Custom function"))
  :group 'content-quoter)

;;; History Management

(defvar content-quoter-history nil
  "History of content quoter source plists.
Each element is a plist with properties:
:content-plists - The list of plists representing the sources.
:directory - The working directory when sources were collected.
:timestamp - When the sources were collected.
:description - A human-readable description of the source set.")

(defun content-quoter--format-plists-description (content-plists)
  "Create a descriptive string for a list of CONTENT-PLISTS."
  (let ((source-names (mapcar (lambda (plist)
                                (format "%s: %s"
                                        (plist-get plist :type)
                                        (plist-get plist :name)))
                              content-plists)))
    (format "[%s] %s"
            (format-time-string "%Y-%m-%d %H:%M")
            (concat
             (string-join (seq-take source-names 3) ", ")
             (when (> (length source-names) 3)
               (format " and %d more" (- (length source-names) 3)))))))

(defun content-quoter--make-history-entry (content-plists &optional description)
  "Create a history entry plist for CONTENT-PLISTS.
DESCRIPTION is a human-readable description of the source set."
  (list :content-plists (copy-sequence content-plists)
        :directory default-directory
        :timestamp (current-time)
        :description (or description
                         (content-quoter--format-plists-description
                          content-plists))))

(defun content-quoter-add-to-history (content-plists &optional description)
  "Add CONTENT-PLISTS to `content-quoter-history'.
DESCRIPTION is a human-readable description of the source set."
  (when content-plists ; Don't add empty sets
    (push (content-quoter--make-history-entry content-plists description)
          content-quoter-history)))

(defun content-quoter--move-to-history-top (description)
  "Move the history entry with DESCRIPTION to the top of the history."
  (let ((entry (assoc description
                      (mapcar (lambda (e) (cons (plist-get e :description) e))
                              content-quoter-history))))
    (when entry
      (setq content-quoter-history
            (cons (cdr entry)
                  (delq (cdr entry) content-quoter-history))))))

(defun content-quoter-select-from-history ()
  "Select a history entry using completion and return its content-plists.
Returns nil if no history or selection cancelled.
The selected item becomes the most recent in history."
  (when content-quoter-history
    (let* ((candidates (mapcar (lambda (entry)
                                 (cons (plist-get entry :description) entry))
                               content-quoter-history))
           (default (caar candidates))
           (prompt (format "Select previous sources (default %s): "
                           (or default "none")))
           (choice (completing-read prompt candidates nil t nil nil default)))
      (when (and choice (not (string-empty-p choice)))
        (content-quoter--move-to-history-top choice)
        (let ((entry (cdr (assoc choice candidates))))
          (when entry
            (let ((default-directory (plist-get entry :directory)))
              (plist-get entry :content-plists))))))))

;;; Content Fetching and Structuring

(defun content-quoter--ensure-trailing-newline (content)
  "Ensure that CONTENT ends with exactly one newline."
  (if (string-suffix-p "\n" content)
      content
    (concat content "\n")))

(defun content-quoter--mode-to-language (mode-symbol)
  "Convert MODE-SYMBOL name to a language string for syntax highlighting."
  (replace-regexp-in-string "-mode\\'" "" (symbol-name mode-symbol)))

(defun content-quoter--get-buffer-content-plist (buffer)
  "Get content plist from BUFFER."
  (with-current-buffer buffer
    (list :content (content-quoter--ensure-trailing-newline
                    (buffer-substring-no-properties (point-min) (point-max)))
          :language (content-quoter--mode-to-language major-mode)
          :type 'buffer
          :name (buffer-name))))

(defun content-quoter--get-file-content-plist (filepath)
  "Get content plist from file at FILEPATH."
  (condition-case err
      (with-temp-buffer
        (setq buffer-file-name filepath)
        (insert-file-contents filepath)
        (normal-mode)
        (setq buffer-file-name nil)
        (list :content (content-quoter--ensure-trailing-newline (buffer-string))
              :language (content-quoter--mode-to-language major-mode)
              :type 'file
              :name (file-relative-name filepath default-directory)))
    (error (message "Warning: Cannot read file %s: %s" filepath err) nil)))

(defun content-quoter--get-region-content-plist (buffer start end)
  "Get content plist for region (START . END) in BUFFER."
  (with-current-buffer buffer
    (list :content (content-quoter--ensure-trailing-newline
                    (buffer-substring-no-properties start end))
          :language (content-quoter--mode-to-language major-mode)
          :type 'region
          :name (format "%s (%d-%d)" (buffer-name) start end))))

(defun content-quoter--get-content-plists (sources)
  "Convert a list of SOURCES (buffer names, file paths) into content plists.
SOURCES can contain buffer objects, buffer names (strings), or file
paths (strings)."
  (let (content-plists)
    (dolist (source sources)
      (let ((plist
             (cond
              ((bufferp source)
               (content-quoter--get-buffer-content-plist source))
              ((and (stringp source) (get-buffer source)) ; Buffer name
               (content-quoter--get-buffer-content-plist (get-buffer source)))
              ((and (stringp source) (file-exists-p source)) ; File path
               (content-quoter--get-file-content-plist source))
              (t
               (message
                "Warning: Source identifier '%s' not found or invalid" source)
               nil))))
        (when plist (push plist content-plists))))
    (nreverse content-plists)))

;;; Source Collection Helpers

(defun content-quoter--collect-visible-buffers ()
  "Get list of unique visible buffer objects."
  (seq-uniq (mapcar #'window-buffer (window-list))))

(defun content-quoter--collect-git-files (wildcard)
  "Get list of git-tracked files matching WILDCARD pattern.
Returns absolute file paths relative to `default-directory` or git root."
  (let ((git-root (locate-dominating-file default-directory ".git")))
    (unless git-root
      (user-error "Not in a git repository"))
    (with-temp-buffer
      (let ((default-directory git-root) ; Run git command from root
            (exit-code (call-process "git" nil t nil "ls-files" "--full-name")))
        (unless (zerop exit-code)
          (user-error "Failure: git ls-files with code %d" exit-code))
        (let* ((git-files (split-string (buffer-string) "\n" t))
               (matching-files
                (if (or (null wildcard) (string= wildcard "*.*"))
                    git-files
                  (let ((regexp (wildcard-to-regexp wildcard)))
                    (seq-filter (lambda (file) (string-match-p regexp file))
                                git-files)))))
          ;; Return absolute paths
          (mapcar (lambda (file) (expand-file-name file git-root))
                  matching-files))))))

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
                              (string-match-p
                               regexp
                               (file-relative-name file (project-root pr))))
                            all-files)))))
      matching-files)))

(defun content-quoter--collect-directory-files (dir
                                                &optional wildcard recursive)
  "Get list of files in DIR matching optional WILDCARD.
Handles RECURSIVE search.  Returns absolute file paths."
  (let* ((regexp (if (or (null wildcard) (string= wildcard "*.*"))
                     nil
                   (wildcard-to-regexp wildcard)))
         (files (directory-files dir t regexp nil recursive)))
    (if recursive
        (seq-filter (lambda (f) (not (file-directory-p f))) files)
      files)))

(defun content-quoter--collect-selected-buffers ()
  "Prompt user to select multiple buffers.  Returns list of buffer objects."
  (let ((selected-names
         (completing-read-multiple "Select buffers to quote: "
                                   (mapcar #'buffer-name (buffer-list))
                                   nil t)))
    (mapcar #'get-buffer selected-names)))

(defun content-quoter--collect-dired-marked-files ()
  "Get list of marked files in Dired, or file at point if none marked.
Returns absolute file paths."
  (unless (eq major-mode 'dired-mode)
    (user-error "Not in a Dired buffer"))
  (dired-get-marked-files))

;;; Main Processing and User Commands

(defun content-quoter--format-plists-to-string (content-plists wrapper-func)
  "Format a list of CONTENT-PLISTS using WRAPPER-FUNC and combine results."
  (if content-plists
      (string-join (mapcar wrapper-func content-plists) "\n")
    ""))

(defun content-quoter--process-and-copy (content-plists source-description)
  "Format CONTENT-PLISTS, copy to clipboard, add to history.
SOURCE-DESCRIPTION is a human-readable description of the sources."
  (if content-plists
      (let ((final-content (content-quoter--format-plists-to-string
                            content-plists content-quoter-wrapper))
            (count (length content-plists)))
        (kill-new final-content)
        ;; Add to history *after* successful processing
        (content-quoter-add-to-history content-plists)
        (message "Copied content from %d source%s (%s)"
                 count (if (= count 1) "" "s") source-description))
    (message "No content collected.")))

(defun content-quoter--read-wildcard ()
  "Prompt user for a file wildcard pattern."
  (read-string "File wildcard (default: *.*): " nil nil "*.*"))

;;;###autoload
(defun content-quoter-current-buffer-to-clipboard ()
  "Quote content of the current buffer and copy to clipboard."
  (interactive)
  (let ((plists (list (content-quoter--get-buffer-content-plist
                       (current-buffer)))))
    (content-quoter--process-and-copy plists "current buffer")))

;;;###autoload
(defun content-quoter-visible-buffers-to-clipboard ()
  "Quote content of all unique visible buffers and copy to clipboard."
  (interactive)
  (let* ((buffers (content-quoter--collect-visible-buffers))
         (plists (content-quoter--get-content-plists buffers)))
    (content-quoter--process-and-copy plists "visible buffers")))

;;;###autoload
(defun content-quoter-selected-buffers-to-clipboard ()
  "Prompt to select buffers, quote their content, and copy to clipboard."
  (interactive)
  (let* ((buffers (content-quoter--collect-selected-buffers))
         (plists (content-quoter--get-content-plists buffers)))
    (content-quoter--process-and-copy plists "selected buffers")))

;;;###autoload
(defun content-quoter-git-files-to-clipboard (&optional wildcard)
  "Quote content of git-tracked files matching WILDCARD and copy to clipboard."
  (interactive (list (content-quoter--read-wildcard)))
  (let* ((files (content-quoter--collect-git-files wildcard))
         (plists (content-quoter--get-content-plists files)))
    (content-quoter--process-and-copy
     plists
     (format "git files%s" (if (string= wildcard "*.*") ""
                             (format " matching %s" wildcard))))))

;;;###autoload
(defun content-quoter-project-files-to-clipboard (&optional wildcard)
  "Quote content of project files matching WILDCARD and copy to clipboard."
  (interactive (list (content-quoter--read-wildcard)))
  (let* ((files (content-quoter--collect-project-files wildcard))
         (plists (content-quoter--get-content-plists files)))
    (content-quoter--process-and-copy
     plists
     (format "project files%s" (if (string= wildcard "*.*") ""
                                 (format " matching %s" wildcard))))))

;;;###autoload
(defun content-quoter-directory-files-to-clipboard (dir &optional
                                                        wildcard recursive)
  "Quote content of files from DIR matching WILDCARD and copy to clipboard.
With prefix arg RECURSIVE, include subdirectories."
  (interactive
   (list (read-directory-name "Select directory: ")
         (read-string "File wildcard (optional, e.g., *.py): " nil nil "*.*")
         current-prefix-arg))
  (let* ((files (content-quoter--collect-directory-files
                 dir wildcard recursive))
         (plists (content-quoter--get-content-plists files)))
    (content-quoter--process-and-copy
     plists
     (format "%s files in %s%s%s"
             (if recursive "recursive" "directory")
             (abbreviate-file-name dir)
             (if (string= wildcard "*.*") "" (format " matching %s" wildcard))
             (if recursive "")))))

;;;###autoload
(defun content-quoter-dired-marked-files-to-clipboard ()
  "Quote content of marked files in Dired and copy to clipboard.
If no files marked, quote file at point."
  (interactive)
  (let* ((files (content-quoter--collect-dired-marked-files))
         (plists (content-quoter--get-content-plists files)))
    (content-quoter--process-and-copy plists "Dired marked files")))

;;;###autoload
(defun content-quoter-from-history-to-clipboard ()
  "Select a previous set of quoted sources from history and copy to clipboard."
  (interactive)
  (let ((plists (content-quoter-select-from-history)))
    (if plists
        (content-quoter--process-and-copy plists "from history")
      (message "History selection cancelled or history empty."))))

;;;###autoload
(defun content-quoter-dwim (&optional arg)
  "Do-what-I-mean for content quoting and copy to clipboard.
With prefix ARG, quote visible buffers.
With active region, quote only the region.
Otherwise, quote the current buffer."
  (interactive "P")
  (let (plists source-description)
    (cond (arg
           (setq plists (content-quoter--get-content-plists
                         (content-quoter--collect-visible-buffers))
                 source-description "visible buffers (DWIM)"))
          ((use-region-p)
           (setq plists (list (content-quoter--get-region-content-plist
                               (current-buffer)
                               (region-beginning) (region-end)))
                 source-description "region (DWIM)"))
          (t
           (setq plists (list (content-quoter--get-buffer-content-plist
                               (current-buffer)))
                 source-description "current buffer (DWIM)")))
    (content-quoter--process-and-copy plists source-description)))


(provide 'content-quoter)
;;; content-quoter.el ends here
