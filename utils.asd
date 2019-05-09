#|
  This file is a part of utils project.
  Copyright (c) 2019 Innaky (innaky@protonmail.com)
|#

#|
  Author: Innaky (innaky@protonmail.com)
|#

(defsystem "utils"
  :version "0.1.0"
  :author "Innaky"
  :license "GPLv3"
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "utils"))))
  :description "Utilities"
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.markdown"))
  :in-order-to ((test-op (test-op "utils-test"))))
