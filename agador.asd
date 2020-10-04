;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

(defsystem "agador"
  :description "Natural language experiments using b-trees"
  :depends-on (:LMDB :CL-UTILITIES :BABEL :CL-PPCRE :USOCKET
		:LOG4CL SB-CONCURRENCY :ALEXANDRIA
		:CL-DATE-TIME-PARSER :UNIX-OPTS :dexador
		:plump :lquery)

  :components (
    (:file "package")
    (:file "classes")
    (:file "main" :depends-on ("package"))

    (:module "util"
	     :depends-on ("package")
	     :serial T
      :components (
		   (:file "terminal")
		   (:file "mailbox")
		   (:file "net")
		   (:file "schedule")
		   (:file "sha1")))
    (:module "speech"
	     :depends-on ("package")
	     :serial T
	     :components (
		     (:file "vosk")
		     (:file "talk")))

    (:module "gui"
	     :depends-on ("package" "classes")
	     :serial T
	     :components (
			  (:file "windows")
		   ))
    (:module "parser"
	     :depends-on ("package" "classes")
	     :serial T
      :components (
        (:file "pclass")
	(:file "rules")
	(:file "words")
;;	(:file "number")
	(:file "display")
	(:file "parse")
	(:file "corpus")
	)
      )

    (:module "action"
	     :depends-on ("package" "classes")
	     :serial T
      :components (
		   (:file "dispatch")
		   (:file "time")
		   (:file "weather")
		   (:file "security")))

    (:module "memory"
	     :depends-on ("package" "classes")
	     :serial T	       
      :components (
	(:file "mclass")
	(:file "db")
	(:file "tree")
	(:file "explore")))

    )
  )
