;;; lsj-abbrev.el --- 

;; Copyright 2017 double
;;
;; Author: double@double
;; Version: $Id: lsj-abbrev.el,v 0.0 2017/04/06 11:10:59 double Exp $
;; Keywords: 
;; X-URL: not distributed yet

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; 

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'lsj-abbrev)

;;; Code:

;; -*- coding: utf-8; lexical-binding: t; -*-
;; emacs abbrev def
;; http://ergoemacs.org/emacs/emacs_abbrev_mode.html

(clear-abbrev-table global-abbrev-table)

(define-abbrev-table 'global-abbrev-table
  '(

    ;; phrase
    ("afaik" "as far as i know" )
    ("atm" "at the moment" )
    ("dfb" "difference between" )
    ("ty" "thank you" )
    ("gitaddupstream" "git remote add upstream git@172.16.0.2:parallel_world/parallel_world.git")
    ("ks" "killall -9 server")
    ("server" "cd ~/parallel_world/server")
    ("gopw" "cd ~/parallel_world/gopw")
    ("pw" "cd ~/parallel_world")
    ))

(set-default 'abbrev-mode t)

(setq save-abbrevs nil)



(provide 'lsj-abbrev)
(eval-when-compile
  (require 'cl))



;;;;##########################################################################
;;;;  User Options, Variables
;;;;##########################################################################





;;; lsj-abbrev.el ends here
