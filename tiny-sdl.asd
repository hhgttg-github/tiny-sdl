
;;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage tiny-sdl
  (:use :cl :asdf :alexandria))

(in-package tiny-sdl)

(defsystem tiny-sdl
  :name "tiny-sdl"
  :version "0.0.1"
  :maintainer "ks"
  :author "ks"
  :serial t
;  :depends-on ("sprite")
;  :depends-on ("font")
  :components
  ((:file "message")
   (:file "2d-maze")
   (:file "table")
   (:file "main" :depends-on ("message" "table"))))
