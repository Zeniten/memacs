;; Inspiration:
;; https://github.com/magnars/emacsd-reboot/blob/main/settings/fast-startup.el
;; https://github.com/doomemacs/doomemacs/blob/master/early-init.el
;; https://github.com/doomemacs/doomemacs/blob/develop/docs/faq.org#how-does-doom-start-up-so-quickly

(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)

;; ... your emacs config here ...
;; Set file-name-handler-alist
;; TODO Why?
(setq file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

(blink-cursor-mode -1)        ; disable blinking cursor
(menu-bar-mode -1)            ; disable menu bar
(scroll-bar-mode -1)          ; disable scroll bar
(tool-bar-mode -1)            ; disable tool bar (big buttons)
(tooltip-mode -1)             ; disable tooltips

;; Set language and encoding early
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

;; Inhibit splash screen and set up initial buffer behavior
(setq inhibit-startup-screen t          ; skip startup screen
      initial-major-mode 'fundamental-mode ; faster startup
      initial-scratch-message nil       ; clean scratch buffer
      ring-bell-function 'ignore        ; quiet
      )

;; (run-with-idle-timer
;;    5 nil
;;    (lambda ()
;;      (setq gc-cons-threshold 800000)
;;      (setq file-name-handler-alist file-name-handler-alist-original)
;;      (makunbound 'file-name-handler-alist-original)))


;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
;; - https://github.com/LionyxML/emacs-solo/blob/main/early-init.el
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold 16777216 ; 16mb
          gc-cons-percentage 0.1)))
