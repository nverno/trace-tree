;;; trace-tree.el --- Major mode to visualize elisp trace output as tree widget -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/trace-tree
;; Package-Requires:
;; Created:  10 April 2023

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;;; Description:
;;
;;  Major mode to view elisp trace output using interactive tree widgets
;;  (collapse/expand children).
;;
;;  Overrides `trace-make-advice' from trace.el.
;;
;;; Installation:
;;
;;  When this file is loaded, it will simply override (using advice)
;;  `trace-make-advice' so the output from the `trace-*' functions in trace.el will
;;  be handled by this mode, instead of as plaintext.
;;
;;  Place file on `load-path' and `require' somewhere before
;;  calling tracing functions.
;;
;;; Code:
(eval-when-compile
  (require 'cl-lib))
(require 'dash)
(require 'tree-widget)
(require 'trace)

(defvar trace-results nil)

(defvar trace--node nil)

(cl-defstruct trace-node
  (function nil)
  (args nil)
  (entry-context nil)
  (exit-context nil)
  (result nil)
  (level 0)
  (children ()))

(defun trace-tree-buffer (name)
  "Get trace output buffer."
  (if-let* ((buf (get-buffer name)))
      buf
    (with-current-buffer (get-buffer-create name)
      (trace-tree-mode)
      (current-buffer))))

;;; modified `trace-make-advice' to build trace tree
(defun trace-tree-make-advice (function buffer background context)
  "Build the piece of advice to be added to trace FUNCTION.
FUNCTION is the name of the traced function.
BUFFER is the buffer where the trace should be printed.
BACKGROUND if nil means to display BUFFER.
CONTEXT if non-nil should be a function that returns extra info that should
be printed along with the arguments in the trace."
  (lambda (body &rest args)
    (let* ((trace-level (1+ trace-level))
           (trace-buffer (trace-tree-buffer buffer))
           (deactivate-mark nil)         ;Protect deactivate-mark.
           (ctx (funcall context))
           (parent trace--node)
           (trace--node (make-trace-node
                         :function function
                         :level trace-level
                         :entry-context ctx
                         :args args)))
      (let ((result))
        (unwind-protect
            (setq result (list (apply body args)))
          (unless inhibit-trace
            (let ((ctx (funcall context)))
              (setf (trace-node-exit-context trace--node) ctx)
              (setf (trace-node-result trace--node)
                    (if result (car result) '\!non-local\ exit\!))
              (with-current-buffer trace-buffer
                (if parent
                    (push trace--node (trace-node-children parent))
                  (unless background (trace--display-buffer trace-buffer))
                  (goto-char (point-max))
                  (setq trace-results (cons trace--node trace-results))
                  (trace-tree-render-trace trace--node))))))
        (car result)))))


;; Override `trace-make-advice' to adapt tracing functions to create tree
;;;###autoload
(defun trace-tree-enable ()
  "Override `trace-make-advice' to adapt tracing functions to create tree."
  (interactive)
  (advice-add 'trace-make-advice :override #'trace-tree-make-advice))

(defun trace-tree-disable ()
  "Remove overrides."
  (interactive)
  (advice-remove 'trace-make-advice #'trace-tree-make-advice))

(defun trace-tree-render-trace (trace-result)
  (widget-create (trace-tree-render-node trace-result)))

;;; XXX: better format for function arguments/results
;;       hide/show lengthy ones
(defun trace-tree-render-node (node)
  `(tree-widget
    :tag ,(format "%S%s -> %S%s"
                  (cons (trace-node-function node) (trace-node-args node))
                  (trace-node-entry-context node)
                  (trace-node-result node)
                  (trace-node-exit-context node))
    :open t
    ,@(->> node
           (trace-node-children)
           (-map 'trace-tree-render-node)
           (nreverse))
    ;; (tree-widget
    ;;  :tag ,(format "<- %S%s" (trace-node-result node) (trace-node-exit-context node))
    ;;  :icon nil)
    ))

(defun trace-tree-revert-buffer (_ignore-auto _noconfirm)
  "Rebuild trace output."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (--each-r trace-results (widget-create (trace-tree-render-node it))))
  (widget-setup))

(defun trace-tree-collapse-all (&optional arg)
  "Collapse all top-level trace nodes.
With ARG, expand all top-level trace nodes."
  (interactive "P" trace-tree-mode)
  (save-excursion
    (goto-char (point-min))
    (while (condition-case nil
               (progn
                 (when (xor arg
                            (widget-get
                             (widget-get (get-char-property (point) 'button) :parent)
                             :open))
                   (widget-button-press (point)))
                 (widget-forward 1)
                 t)
             (error nil)))))

(defun trace-tree-clear-results ()
  "Clear trace results."
  (interactive)
  (when-let* ((buf (get-buffer trace-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer))))
  (setq trace-results nil))

;;; XXX: widget interactions: untrace function/goto source
;; D => delete node
;; list currently traced functions
(defvar-keymap trace-tree-mode-map
  :doc "Keymap used in trace output buffer."
  :parent (make-composed-keymap special-mode-map widget-keymap)
  "n"       #'next-line
  "p"       #'previous-line
  "C"       #'trace-tree-collapse-all
  "C-c C-q" #'untrace-all
  "C-c C-k" #'trace-tree-clear-results)

(define-derived-mode trace-tree-mode special-mode "TraceOutput"
  "Major mode for displaying trace results.

\\{trace-tree-mode-map}"
  :interactive nil
  :syntax-table emacs-lisp-mode-syntax-table
  :abbrev-table nil
  (setq trace-results nil)
  (setq truncate-lines t)
  (setq-local revert-buffer-function #'trace-tree-revert-buffer))

(provide 'trace-tree)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; trace-tree.el ends here
