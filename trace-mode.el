;;; trace-mode.el --- Major mode to visualize elisp trace output -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/trace-mode
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
;;  Major mode to view elisp trace output.
;;
;;; Installation:
;;
;; ```lisp
;; ```
;;
;;; Code:
(eval-when-compile
  (require 'cl-lib))
(require 'dash)
(require 'tree-widget)
(require 'trace)

(defvar-local trace-results nil)

(defvar trace--node nil)

(cl-defstruct trace-node
  (function nil)
  (args nil)
  (entry-context nil)
  (exit-context nil)
  (result nil)
  (level 0)
  (children ()))

;;; modified `trace-mode-advice' to build trace tree
(defun trace-mode-make-advice (function buffer background context)
  "Build the piece of advice to be added to trace FUNCTION.
FUNCTION is the name of the traced function.
BUFFER is the buffer where the trace should be printed.
BACKGROUND if nil means to display BUFFER.
CONTEXT if non-nil should be a function that returns extra info that should
be printed along with the arguments in the trace."
  (lambda (body &rest args)
    (let* ((trace-level (1+ trace-level))
           (trace-buffer (get-buffer-create buffer))
           (deactivate-mark nil)         ;Protect deactivate-mark.
           (ctx (funcall context))
           (parent trace--node)
           (trace--node (make-trace-node
                          :function function
                          :level trace-level
                          :entry-context ctx
                          :args args)))
      (unless inhibit-trace
        (with-current-buffer trace-buffer
          (setq-local window-point-insertion-type t)
          (unless background (trace--display-buffer trace-buffer))
          (goto-char (point-max))
          ;; Insert a separator from previous trace output:
          (if (= trace-level 1) (insert trace-separator))
          (insert
           (trace-entry-message
            function trace-level args ctx))))
      (let ((result))
        (unwind-protect
            (setq result (list (apply body args)))
          (unless inhibit-trace
            (let ((ctx (funcall context)))
              (with-current-buffer trace-buffer
                (unless background (trace--display-buffer trace-buffer))
                (setf (trace-node-exit-context trace--node) ctx)
                (setf (trace-node-result trace--node) result)
                (if parent
                    (push trace--node (trace-node-children parent))
                  (setq trace-results trace--node))
                (goto-char (point-max))
                (insert
                 (trace-exit-message
                  function
                  trace-level
                  (if result (car result) '\!non-local\ exit\!)
                  ctx))))))
        (car result)))))


(defun trace-mode-function-internal (function buffer background context)
  "Add trace advice for FUNCTION."
  (advice-add
   function :around
   (trace-mode-make-advice function (or buffer trace-buffer) background
                           (or context (lambda () "")))
   `((name . ,trace-advice-name) (depth . -100))))

(defun trace-mode-function-foreground (function &optional buffer context)
  (interactive (trace--read-args "Trace function"))
  (trace-mode-function-internal function buffer nil context))

(defun trace-mode-render-node (node)
  `(tree-widget :tag ,(format "%S" (trace-node-function node))
                :open t
                ,@(->> node
                       (trace-node-children)
                       (-map 'trace-mode-render-node))))

(defun trace-mode-tree ()
  (interactive)
  (when-let (buf (get-buffer trace-buffer))
    (with-current-buffer buf
      (let ((results trace-results))
        (let ((inhibit-read-only t))
          (trace-mode)
          (erase-buffer)
          (widget-create
           `(tree-widget
             :tag ,(format "%S" (trace-node-function results))
             :open t
             ,@(->> results
                    (trace-node-children)
                    (-map 'trace-mode-render-node)))))))))


(define-derived-mode trace-mode special-mode "TraceOutput"
  "Major mode for displaying trace results.")

(provide 'trace-mode)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; trace-mode.el ends here
