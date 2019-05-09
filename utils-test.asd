#|
  This file is a part of utils project.
  Copyright (c) 2019 Innaky (innaky@protonmail.com)
|#

(defsystem "utils-test"
  :defsystem-depends-on ("prove-asdf")
  :author "Innaky"
  :license "GPLv3"
  :depends-on ("utils"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "utils"))))
  :description "Test system for utils"

  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
