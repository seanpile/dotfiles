;;; init.el -*- lexical-binding: t; -*-
;; Copy me to ~/.doom.d/init.el or ~/.config/doom/init.el, then edit me!

(doom! :feature
      ;debugger          ; FIXME stepping through code, to help you add bugs
       eval              ; run code, run (also, repls)
       (evil +everywhere); come to the dark side, we have cookies
       lookup            ; helps you navigate your code and documentation

       :completion
       company           ; the ultimate code completion backend
       (ivy              ; a search engine for love and life
        +fuzzy)          ; enable fuzzy search backend for ivy

       :ui
       hl-todo           ; highlight TODO/FIXME/NOTE tags
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB

       :emacs
       ;eshell            ; a consistent, cross-platform shell (WIP)
       ;term              ; terminals in Emacs

       :tools
       ;magit             ;

       :lang
       emacs-lisp        ; drown in parentheses
       go                ; the hipster dialect
       ;assembly          ; assembly for fun or debugging
       ;cc                ; C/C++/Obj-C madness
       ;crystal           ; ruby at the speed of c
       ;clojure           ; java with a lisp
       ;csharp            ; unity, .NET, and mono shenanigans
       ;data              ; config/data formats
       ;erlang            ; an elegant language for a more civilized age
       ;elixir            ; erlang done right
       ;elm               ; care for a cup of TEA?
       ;ess               ; emacs speaks statistics
       ;(haskell +intero) ; a language that's lazier than I am
       ;hy                ; readability of scheme w/ speed of python
       ;(java +meghanada) ; the poster child for carpal tunnel syndrome
       ;javascript        ; all(hope(abandon(ye(who(enter(here))))))
       ;julia             ; a better, faster MATLAB
       ;latex             ; writing papers in Emacs has never been so fun
       ;ledger            ; an accounting system in Emacs
       ;lua               ; one-based indices? one-based indices
       ;markdown          ; writing docs for people to ignore
       ;nim               ; python + lisp at the speed of c
       ;nix               ; I hereby declare "nix geht mehr!"
       ;ocaml             ; an objective camel
       ; (org              ; organize your plain life in plain text
       ;  +attach          ; custom attachment system
       ;  +babel           ; running code in org
       ;  +capture         ; org-capture in and outside of Emacs
       ;  +export          ; Exporting org to whatever you want
       ;  +present)        ; Emacs for presentations
       ;perl              ; write code no one else can comprehend
       ;php               ; perl's insecure younger brother
       ;plantuml          ; diagrams for confusing people more
       ;purescript        ; javascript, but functional
       ;python            ; beautiful is better than ugly
       ;rest              ; Emacs as a REST client
       ;ruby              ; 1.step do {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       ;rust              ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       ;scala             ; java, but good
       ;sh                ; she sells (ba|z)sh shells on the C xor
      ;solidity          ; do you need a blockchain? No.
      ;swift             ; who asked for emoji variables?
      ;web               ; the tubes

       ;; Applications are complex and opinionated modules that transform Emacs
       ;; toward a specific purpose. They may have additional dependencies and
       ;; should be loaded late.
       :app
       ;(email +gmail)    ; emacs as an email client
       ;irc               ; how neckbeards socialize
       ;(rss +org)        ; emacs as an RSS reader
       ;twitter           ; twitter client https://twitter.com/vnought
       ;(write            ; emacs as a word processor (latex + org + markdown)
       ; +wordnut         ; wordnet (wn) search
       ; +langtool)       ; a proofreader (grammar/style check) for Emacs

       :collab
       ;floobits          ; peer programming for a price
       ;impatient-mode    ; show off code over HTTP

       :private
       (seanpile +bindings +evil-commands))

