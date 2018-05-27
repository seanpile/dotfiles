;;; init.el -*- lexical-binding: t; -*-
;; Copy me to ~/.doom.d/init.el or ~/.config/doom/init.el, then edit me!

(doom! :feature
      ;debugger          ; FIXME stepping through code, to help you add bugs
       (evil +everywhere); come to the dark side, we have cookies
       lookup            ; helps you navigate your code and documentation

       :completion
       (company          ; the ultimate code completion backend
        +childframe)     ; a nicer company UI (Emacs 26+ only)

       ivy 		 ; a search engine for love and life

       :ui
       hl-todo           ; highlight TODO/FIXME/NOTE tags
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB

       :tools
       electric-indent
       magit

       :lang
       go                ; the hipster dialect
       emacs-lisp        ; drown in parentheses
       markdown          ; writing docs for people to ignore
       data              ; config/data formats
       javascript        ; all(hope(abandon(ye(who(enter(here))))))
       ; (cc +irony)       ; C/C++/Obj-C madness
       ;rust              ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       ;sh                ; she sells (ba|z)sh shells on the C xor

       ; Additional disabled languages
       ;  assembly          ; assembly for fun or debugging
       ;  crystal           ; ruby at the speed of c
       ;  clojure           ; java with a lisp
       ;  csharp            ; unity, .NET, and mono shenanigans
       ;  erlang            ; an elegant language for a more civilized age
       ;  elixir            ; erlang done right
       ;  elm               ; care for a cup of TEA?
       ;  ess               ; emacs speaks statistics
       ;  (haskell +intero) ; a language that's lazier than I am
       ;  hy                ; readability of scheme w/ speed of python
       ;  (java +meghanada) ; the poster child for carpal tunnel syndrome
       ;  julia             ; a better, faster MATLAB
       ;  latex             ; writing papers in Emacs has never been so fun
       ;  ledger            ; an accounting system in Emacs
       ;  lua               ; one-based indices? one-based indices
       ;  nim               ; python + lisp at the speed of c
       ;  nix               ; I hereby declare "nix geht mehr!"
       ;  ocaml             ; an objective camel
       ;  (org              ; organize your plain life in plain text
        ;  +attach          ; custom attachment system
        ;  +babel           ; running code in org
        ;  +capture         ; org-capture in and outside of Emacs
        ;  +export          ; Exporting org to whatever you want
        ;  +present         ; Emacs for presentations
        ;  +publish)        ; Emacs+Org as a static site generator
       ;  perl              ; write code no one else can comprehend
       ;  php               ; perl's insecure younger brother
       ;  plantuml          ; diagrams for confusing people more
       ;  purescript        ; javascript, but functional
       ;  python            ; beautiful is better than ugly
       ;  rest              ; Emacs as a REST client
       ;  ruby              ; 1.step do {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       ;  scala             ; java, but good
       ;  swift             ; who asked for emoji variables?
       ;  typescript        ; javascript, but better
       ;  web               ; the tubes

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

       :private
       (seanpile +bindings +evil-commands))
