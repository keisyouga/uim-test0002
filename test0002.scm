(require-extension (srfi 48))
(require-dynlib "test0002")


;;; user configs

;; key defs


(require "generic-key-custom.scm")

;; FIXME: using *-custom.scm
(define-key test0002-on-key? 'generic-on-key?)
(define-key test0002-off-key? 'generic-off-key?)
(define-key test0002-backspace-key? 'generic-backspace-key?)
(define-key test0002-cancel-key? 'generic-cancel-key?)
;;(define-key test0002-commit-key? 'generic-commit-key?)
(define-key test0002-commit-key? '(" " generic-commit-key?))
;;(define-key test0002-begin-conv-key? 'generic-begin-conv-key?)

(define-key test0002-next-candidate-key? 'generic-next-candidate-key?)
(define-key test0002-prev-candidate-key? 'generic-prev-candidate-key?)
(define-key test0002-next-page-key? '("<Control>f" generic-next-page-key?))
(define-key test0002-prev-page-key? '("<Control>b" generic-prev-page-key?))
(define-key test0002-first-candidate-key? '("<Control>a" "home"))
(define-key test0002-last-candidate-key? '("<Control>e" "end"))
(define test0002-use-candidate-window? #t)
(define test0002-candidates-max 10)

(define alist-get
  (lambda (alist key)
    (cdr (or (assq key alist) '(#f . #f)))))

(define alist-set!
  (lambda (alist key value)
    (let ((target (assq key alist)))
      (if target
          (set-cdr! target value)
          ;; if no key, add new cell (key . value) to alist
          (begin
            (set-cdr! alist (cons (car alist) (cdr alist)))
            (set-car! alist (cons key value)))))))

;; widgets and actions

;; widgets
(define test0002-widgets '(widget_test0002_input_mode))

;; default activity for each widgets
(define default-widget_test0002_input_mode 'action_test0002_off)

;; actions of widget_test0002_input_mode
(define test0002-input-mode-actions
  '(action_test0002_off
    action_test0002_on))


;;; implementations

(register-action 'action_test0002_off
                 (lambda (context)
                   (list
                    'off
                    "-"
                    (N_ "off")
                    (N_ "Direct Input Mode")))
                 (lambda (context)
                   (not (test0002-context-on context)))
                 #f)

(register-action 'action_test0002_on
                 (lambda (context)
                   (let* ((im (test0002-context-im context))
                          (name (symbol->string (im-name im))))
                     (list
                      'on
                      "O"
                      (N_ "on")
                      (string-append name (N_ " Mode")))))
                 (lambda (context)
                   (test0002-context-on context))
                 #f)

;; Update widget definitions based on action configurations. The
;; procedure is needed for on-the-fly reconfiguration involving the
;; custom API
(define test0002-configure-widgets
  (lambda ()
    (register-widget 'widget_test0002_input_mode
                     (activity-indicator-new test0002-input-mode-actions)
                     (actions-new test0002-input-mode-actions))))

(define test0002-context-rec-spec
  (append
   context-rec-spec
   '(
     (on                    #f)        ; input state
     ;; text currently being entered
     ;; new character is added by car; type "hello" => ("o" "l" "l" "e" "h")
     (seq ())
     ;; list of candidates (("seq1" . "cand1") ("seq2" . "cand2") ...)
     (cands                 ())
     (cand-nth 0)                       ; selected cand index
     (candidate-window #f)              ; candidate window is displayed now?
     (preferences ((hint . #t)))
     )))
(define-record 'test0002-context test0002-context-rec-spec)
(define test0002-context-new-internal test0002-context-new)

(define test0002-context-new
  (lambda (id im tablefile . preferences)
    (let ((context (test0002-context-new-internal id im))
          )
      ;; (uim-notify-info "test0002-context-new")
      (test0002-lib-open-dic tablefile)
      (test0002-context-set-widgets! context test0002-widgets)
      (let ((prefs (test0002-context-preferences context)))
        (map (lambda (p) (alist-set! prefs (car p) (cdr p))) preferences)
        (test0002-context-set-preferences! context prefs))
      context)))

(define test0002-clear
  (lambda (context)
    (test0002-context-set-cands! context ())
    (test0002-context-set-cand-nth! context 0)
    (test0002-context-set-seq! context ())
    (test0002-close-window context)
    ))

(define test0002-commit
  (lambda (context)
    (im-commit context (test0002-get-preedit-string context))
    (test0002-clear context)))

;;; open candidate window
(define test0002-open-window
  (lambda (context len nr)
    ;; (uim-notify-info (format "test0002-open-window: ~s ~s" len nr))
    (im-activate-candidate-selector context len nr)
    (test0002-context-set-candidate-window! context #t)))

;;; close candidate window
(define test0002-close-window
  (lambda (context)
    ;; (uim-notify-info "test0002-close-window")
    (if (test0002-context-candidate-window context)
        (begin
          (im-deactivate-candidate-selector context)
          (test0002-context-set-candidate-window! context #f)))))

;; ("o" "l" "l" "e" "h") => "hello"
(define test0002-get-seq-string
  (lambda (context)
    (apply string-append
           (reverse (test0002-context-seq context)))))

;; if has cands, return selected cand, otherwise return seq
(define test0002-get-preedit-string
  (lambda (context)
    (let ((str (if (pair? (test0002-context-cands context))
                   (cdr (list-ref
                         (test0002-context-cands context)
                         (test0002-context-cand-nth context)))
                   (test0002-get-seq-string context))))
               str)))

(define test0002-update-preedit
  (lambda (context)
    (im-clear-preedit context)
    (im-pushback-preedit context
                         preedit-underline
                         (test0002-get-preedit-string context))
    (im-update-preedit context)))

(define test0002-proc-off-mode
  (lambda (context key key-state)
    (and
     (if (test0002-on-key? key key-state)
         (begin
           (test0002-context-set-on! context #t)
           #f)
         #t)
     ;;
     (im-commit-raw context))))

(define test0002-update-cands
  (lambda (context)
    (if (null? (test0002-context-seq context))
        #f
        (let* ((seq (test0002-get-seq-string context))
               (cands (test0002-lib-get-cands
                       ;; append "*", if prediction
                       (if (alist-get
                            (test0002-context-preferences context) 'prediction)
                           (string-append seq "*") seq))))
          (test0002-context-set-cands! context cands)
          (test0002-context-set-cand-nth! context 0)
          (if (and test0002-use-candidate-window? (pair? cands))
              (begin
                (test0002-open-window
                 context (length cands) test0002-candidates-max)
                (im-select-candidate context 0))
              (test0002-close-window context))
          ))))

(define test0002-proc-on-mode
  (lambda (context key key-state)
    (cond
     ;; off
     ((test0002-off-key? key key-state)
      (test0002-clear context)
      (test0002-context-set-on! context #f))
     ;; commit
     ((test0002-commit-key? key key-state)
      (let ((seq (test0002-context-seq context)))
        (if (null? seq)
            (im-commit-raw context)
            (test0002-commit context))))
     ;; backspace
     ((test0002-backspace-key? key key-state)
      (let ((seq (test0002-context-seq context)))
        (if (null? seq)
            (im-commit-raw context)
            (begin
              (test0002-context-set-seq! context (cdr seq))
              (if (null? (test0002-context-seq context))
                  (test0002-clear context)
                  (test0002-update-cands context))
              ))))
     ;; cancel
     ((test0002-cancel-key? key key-state)
      (if (null? (test0002-context-seq context))
          (im-commit-raw context)
          (test0002-clear context)))
     ;; next candidate
     ((test0002-next-candidate-key? key key-state)
      (if (null? (test0002-context-cands context))
          (im-commit-raw context)
          (let ((nr (length (test0002-context-cands context)))
                (n (+ (test0002-context-cand-nth context) 1)))
            (test0002-context-set-cand-nth! context (if (> nr n) n 0))
            (if test0002-use-candidate-window?
                (im-select-candidate context (test0002-context-cand-nth context))))))
     ;; prev candidate
     ((test0002-prev-candidate-key? key key-state)
      (if (null? (test0002-context-cands context))
          (im-commit-raw context)
          (let ((nr (length (test0002-context-cands context)))
                (n (- (test0002-context-cand-nth context) 1)))
            (test0002-context-set-cand-nth! context (if (<= 0 n) n (- nr 1)))
            (if test0002-use-candidate-window?
                (im-select-candidate context (test0002-context-cand-nth context))))))
     ;; next candidate page
     ((test0002-next-page-key? key key-state)
      (if (null? (test0002-context-cands context))
          (im-commit-raw context)
          (if test0002-use-candidate-window?
              (im-shift-page-candidate context #t)
              (let* ((nr (length (test0002-context-cands context)))
                     (nth (test0002-context-cand-nth context))
                     (n (+ nth test0002-nr-candidates-max)))
                (test0002-context-set-cand-nth! context (if (> nr n) n 0)))
              )))
     ;; prev candidate page
     ((test0002-prev-page-key? key key-state)
      (if (null? (test0002-context-cands context))
          (im-commit-raw context)
          (if test0002-use-candidate-window?
              (im-shift-page-candidate context #f)
              (let* ((nr (length (test0002-context-cands context)))
                     (nth (test0002-context-cand-nth context))
                     (n (- nth test0002-nr-candidates-max)))
                (test0002-context-set-cand-nth! context (if (<= 0 n) n (- nr 1))))
              )))
     ;; first candidate
     ((test0002-first-candidate-key? key key-state)
      (if (null? (test0002-context-cands context))
          (im-commit-raw context)
          (begin (test0002-context-set-cand-nth! context 0)
                 (if test0002-use-candidate-window?
                     (im-select-candidate context 0)))))
     ;; last candidate
     ((test0002-last-candidate-key? key key-state)
      (if (null? (test0002-context-cands context))
          (im-commit-raw context)
          (let ((pos (- (length (test0002-context-cands context)) 1)))
            (test0002-context-set-cand-nth! context pos)
            (if test0002-use-candidate-window?
                (im-select-candidate context pos)))))
     ((symbol? key)
      (im-commit-raw context)
      )
     ;; modifier except shift
     ((and (modifier-key-mask key-state)
           (not (shift-key-mask key-state)))
      (im-commit-raw context))
     ;; input character
     (else
      (let ((key-str (charcode->string key))
            (last-cands (test0002-context-cands context))
            (last-cand-nth (test0002-context-cand-nth context)))
        (test0002-context-set-seq! context
                                   (cons key-str (test0002-context-seq context)))
        (test0002-update-cands context)
        (if (alist-get (test0002-context-preferences context) 'autocommit)
            (cond
             ((= (length (test0002-context-cands context)) 1)
              (test0002-commit context))
             ((= (length (test0002-context-cands context)) 0)
              (test0002-context-set-cands! context last-cands)
              (test0002-context-set-cand-nth! context last-cand-nth)
              (test0002-commit context)
              (if (pair? last-cands)    ; avoid infinite loop
                  (test0002-proc-on-mode context key key-state))
              ))
            ))))
    (test0002-update-preedit context)
    ))

(define test0002-key-press-handler
  (lambda (context key key-state)
    (if (ichar-control? key)
        (im-commit-raw context)
        (if (test0002-context-on context)
            (test0002-proc-on-mode context key key-state)
            (test0002-proc-off-mode context key key-state))
        )))

(define test0002-key-release-handler
  (lambda (context key key-state)
    (if (or (ichar-control? key)
            (not (test0002-context-on context)))
        ;; don't discard key release event for apps
        (im-commit-raw context))))

(define test0002-reset-handler
  (lambda (context)
    ;;(uim-notify-info "reset-handler")
    (test0002-clear context)))

;; return the idx'th candidate
(define test0002-get-candidate-handler
  (lambda (context idx accel-enum-hint)
    ;; (uim-notify-info (format "test0002-get-candidate-handler:~d" idx))
    (let* ((cands (test0002-context-cands context))
           (cell (nth idx cands))
           (cand (if (alist-get (test0002-context-preferences context) 'hint)
                     (string-append (cdr cell) ":" (car cell))
                     (string-append (cdr cell)))))
      ;; (uim-notify-info (format "~s" cell))
      (list cand (digit->string (remainder (+ idx 1) 10)) "")
      )))

;;; set selected candidate number
(define test0002-set-candidate-index-handler
  (lambda (context idx)
    ;; (uim-notify-info (format "candidate-index-handler: ~s" idx))
    (test0002-context-set-cand-nth! context idx)
    (test0002-update-preedit context)))

(define test0002-init-handler
  (lambda (id im init-handler)
    ;; (uim-notify-info "init-handler")
    (init-handler id im #f)))

(define test0002-release-handler
  (lambda (context)
    ;; (uim-notify-info "release-handler")
    (test0002-lib-close-dic)))

(define test0002-register-im
  (lambda (name lang code name-label short-desc init-arg)
    (register-im
     name
     lang
     code
     name-label
     short-desc
     init-arg
     test0002-init-handler
     test0002-release-handler
     context-mode-handler
     test0002-key-press-handler
     test0002-key-release-handler
     test0002-reset-handler
     test0002-get-candidate-handler
     test0002-set-candidate-index-handler
     context-prop-activate-handler
     #f  ;; input-string-handler
     #f  ;; focus-in-handler
     #f  ;; focus-out-handler
     #f  ;; place-handler
     #f  ;; displace-handler
     )))

(test0002-configure-widgets)
