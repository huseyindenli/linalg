(defsystem "linalg"
  :version "0.0.1"
  :author "Your Name"
  :license "MIT"
  :depends-on (#:alexandria
               
               #:cl-liballegro
               #:cl-liballegro-nuklear
               #:cl-glut               
               #:livesupport)
  :serial t
  :components ((:module "src"
                :components
                ((:file "package")
		 (:file "aux")
		 (:file "matrix")
		 (:file "constants")
                 (:file "main"))))
  :description "A simple game."
  :defsystem-depends-on (#:deploy)
  :build-operation "deploy-op"
  :build-pathname #P"linalg"
  :entry-point "linalg:main")
