;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

(defsystem "agador"
  :description "Natural language experiments using b-trees"
  :depends-on (:LMDB :SHA1 :CL-UTILITIES :BABEL :CL-PPCRE :USOCKET
		     :UNIX-OPTS :dexador :plump :lquery)

  :components (
    (:file "package")
    (:file "tclass")
    (:file "main" :depends-on ("package"))
    (:file "schedule" :depends-on ("package"))

    (:module "speech"
	     :depends-on ("package")
	     :serial T
	:components (
		     (:file "julius")
		     (:file "talk")
		     (:file "makewords"))
	)

    (:module "parser"
	     :depends-on ("package" "tclass")
	     :serial T
      :components (
        (:file "class")
	(:file "rules")
	(:file "display")
	(:file "parse")
	)
      )

    (:module "action"
	     :depends-on ("package" "tclass")
	     :serial T
      :components (
		   (:file "dispatch")
		   (:file "time")
		   (:file "weather")
	)
      )

    (:module "memory"
	     :depends-on ("package" "tclass")
	     :serial T	       
      :components (
	(:file "mclass")
	(:file "db")
	(:file "words")
	(:file "tree")
	(:file "explore")
	)
    )

    (:module "util"
	     :depends-on ("package")
	     :serial T
      :components (
		   (:file "terminal")
		   (:file "mailbox")
		   (:file "net")
	)
    )
    )
  )
