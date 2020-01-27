;; example for using test0002.scm

(require (string-append (home-directory (user-name))
                        "/.uim.d/plugin/test0002.scm"))

(define hiragana-init-handler
  (lambda (id im arg)
    (test0002-context-new
     id im
     (string-append (home-directory (user-name)) "/.uim.d/plugin/hiragana.txt")
     '(prediction . #t) '(autocommit . #t))))

(test0002-register-im
 'hiragana
 "*"
 "UTF-8"
 (N_ "hiragana")
 (N_ "hiragana input method")
 hiragana-init-handler)

(define katakana-init-handler
  (lambda (id im arg)
    (test0002-context-new
     id im
     (string-append (home-directory (user-name)) "/.uim.d/plugin/katakana.txt")
     '(prediction . #t) '(autocommit . #t))))

(test0002-register-im
 'katakana
 "*"
 "UTF-8"
 (N_ "katakana")
 (N_ "katakana input method")
 katakana-init-handler)
