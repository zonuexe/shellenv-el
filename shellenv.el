;;; -*- coding: utf-8; lexical-binding: t -*-
;;; shellenv.el --- load environment variables of your shell

;; Author: USAMI Kenta <tadsan@zonu.me>
;; URL: https://github.com/zonuexe/load-shellenv-el
;; Version: 0.0.1
;; Keywords: shell

;; This file is NOT part of GNU Emacs.

;;; License:
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;; user definable variables

(defcustom shellenv/path nil
  "Path to shell which you use."
  :group 'shellenv
  :type  'string)

(defcustom shellenv/shell 'bash
  "Name of your shell"
  :group 'shellenv
  :type  '(choice
           (const :tag "ZShell"   'zsh)
           (const :tag "GNU Bash" 'bash)
           (const :tag "Almquist shell"        'ash)
           (const :tag "Debian Almquist shell" 'dash)
           (const :tag "POSIX.1 shell"         'sh)
           ;;(const :tag "Cmd.exe" 'cmd)
           ;;(const :tag "PowerShell" 'ps1)
           (const :tag "Other"    nil)))

(defcustom shellenv/option nil
  "Arguments of run shell when get a environment variable"
  :group 'shellenv
  :type  'string)

(defcustom shellenv/command nil
  "Command to execute with the shell to get a environment variable"
  :group 'shellenv
  :type  'string)

(defcustom shellenv/env-cmd "/usr/bin/env"
  "Path to `env` Command of your system."
  :group 'shellenv
  :type  'string)

(defcustom shellenv/option-alist
  '(
    ("zsh"  . ("-c" "printenv #{env}"))
    ("bash" . ("-c" "printenv #{env}"))
    ("ash"  . ("-c" "echo $#{env}"))
    ("dash" . ("-c" "echo $#{env}"))
    ("sh"   . ("-c" "echo $#{env}"))
    ; ("ps1" . ("" "Get-Item Env:${env}"))
    ; ("cmd" . ("" "%#{env}%"))
    (nil    . ("-c" "echo $")))
  "Alist of default commandline option of shells"
  :group 'shellenv
  :type 'alist)

;; script local functions
;;; string -> [string]
(defun shellenv/.parse-unix-path (p)
  (split-string p ":"))

;;; string -> [string]
(defun shellenv/.parse-dos-path (p)
  (split-string p ";"))

;;; string -> string
;;; (shellenv/.rep-env "PATH" "foo -c #{env}") = > "foo -c PATH"
(defun shellenv/.rep-env (s v)
  (replace-regexp-in-string "#{env}" s v))

;;; string -> string
;;; (shellenv/.path2sh "/path/to/zsh") => "zsh"
(defun shellenv/.path2sh (p)
  (let ((l (and p (car (last (split-string p "/"))))))
    (cond
     ((equal p l)  nil)
     ((equal l "") nil)
     (t            l))))

;;; string -> string
;;; (shellenv/.envstr "bash") => "/usr/bin/env bash"
(defun shellenv/.envstr (s)
  (concat shellenv/env-cmd " " s))

;;; 'a -> string
(defun shellenv/.2str (s)
  (cond
   ((eq s nil)         nil)
   ((symbolp s) (symbol-name s))
   ((stringp s) s)
   (t           nil)))

;;; () -> symbol
(defun shellenv/shell ()
  (let* ((.pt (shellenv/.path2sh shellenv/path))
         (.st (or shellenv/shell (shellenv/.2str .pt)))
         (.opt (car (shellenv/cmdopt .st)))
         (.cmd (car (cdr (shellenv/cmdopt .st)))))
    (concat .st " " .opt " '" .cmd "'" )))

(setq shellenv/shell "zsh")
(shellenv/shell)
(assoc-default (shellenv/shell) shellenv/option-alist)

;;; () -> string
(defun shellenv/cmdopt (s)
  (or shellenv/command
      (assoc-default
       s
       shellenv/option-alist)))

;;; () -> string
(defun shellenv/cmdstr ()
  (concat shellenv/path " "
          shellenv/option " "
          "'" (shellenv/cmd) "'"))
(shellenv/cmdstr)

(defun shellenv ()
  1)

(let* ((zshpath
        (car (split-string
          (shell-command-to-string "/usr/bin/env zsh -c 'printenv PATH'")
          "[\n ]+")))
       (pathlst (shellenv/.parse-unix-path zshpath)))
  (setq-default exec-path pathlst)
  (setq-default eshell-path-env zshpath)
  (setenv "PATH" zshpath))

(provide 'shellenv)
