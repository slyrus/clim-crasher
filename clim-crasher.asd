
(asdf:defsystem :clim-crasher
  :name "clim-crasher"
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :licence "BSD"
  :default-component-class cl-source-file
  :depends-on (mcclim)
  :serial t
  :components
  ((:file "clim-crasher")))
