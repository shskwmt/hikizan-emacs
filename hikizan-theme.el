;;; hikizan-theme --- theme -*- lexical-binding: t; -*-

;;; Commentary:
;;; The theme of hikizan-emacs.

;;; Code:
;;;###theme-autoload
(deftheme hikizan
  :kind 'color-scheme)

(let ((class '((class color) (min-colors 89)))
      (background "color-16")
      (foreground "white")
      (skyblue "SkyBlue2")
      (brightwhite "brightwhite")
      (brightblack "brightblack")
      )
  (custom-theme-set-faces
   'hikizan
   `(default ((,class (:background ,background :foreground ,foreground))))

   ;; Highlighting faces
   `(fringe ((,class (:background ,brightblack))))
   `(highlight ((,class (:foreground ,foreground :background ,brightblack))))
   `(region ((,class (:background ,brightblack))))
   `(secondary-selection ((,class (:background ,brightblack))))
   `(isearch ((,class (:foreground ,foreground :background ,brightblack))))
   `(lazy-highlight ((,class (:background ,brightblack))))
   `(trailing-whitespace ((,class (:background ,brightblack))))

   ;; Mode line faces
   `(mode-line ((,class
   		 (:box (:line-width -1 :style released-button)
   		  :background ,brightblack :foreground ,foreground))))
   `(mode-line-inactive ((,class
   			  (:box (:line-width -1 :style released-button)
   			   :foreground ,foreground))))
   `(compilation-mode-line-fail ((,class (:foreground ,foreground))))
   `(compilation-mode-line-run  ((,class (:foreground ,foreground))))
   `(compilation-mode-line-exit ((,class (:foreground ,foreground))))

   ;; Escape and prompt faces
   `(minibuffer-prompt ((,class (:foreground ,skyblue))))
   `(escape-glyph ((,class (:foreground ,foreground))))
   `(homoglyph ((,class (:foreground ,foreground))))
   `(error ((,class (:foreground ,foreground))))
   `(warning ((,class (:foreground ,foreground))))
   `(success ((,class (:foreground ,foreground))))

   ;; Font lock faces
   `(font-lock-builtin-face ((,class (:foreground ,skyblue))))
   `(font-lock-comment-face ((,class (:foreground ,brightwhite))))
   `(font-lock-constant-face ((,class (:foreground ,skyblue))))
   `(font-lock-function-name-face ((,class (:foreground ,skyblue))))
   `(font-lock-keyword-face ((,class (:foreground ,skyblue))))
   `(font-lock-string-face ((,class (:foreground ,foreground))))
   `(font-lock-type-face ((,class (:foreground ,skyblue))))
   `(font-lock-variable-name-face ((,class (:foreground ,skyblue))))

   ;; Button and link faces
   `(link ((,class (:underline t :foreground ,foreground))))
   `(link-visited ((,class (:underline t :foreground ,foreground))))

   ;; Message faces
   `(message-header-name ((,class (:foreground ,foreground))))
   `(message-header-cc ((,class (:foreground ,foreground))))
   `(message-header-other ((,class (:foreground ,foreground))))
   `(message-header-subject ((,class (:foreground ,foreground))))
   `(message-header-to ((,class (:foreground ,foreground))))
   `(message-cited-text ((,class (:foreground ,foreground))))
   `(message-separator ((,class (:foreground ,foreground))))

   ;; Flyspell faces
   `(flyspell-duplicate ((,class (:underline ,foreground))))
   `(flyspell-incorrect ((,class (:underline ,foreground))))

   ;; Realgud
   `(realgud-overlay-arrow1  ((,class (:foreground ,foreground))))
   `(realgud-overlay-arrow2  ((,class (:foreground ,foreground))))
   `(realgud-overlay-arrow3  ((,class (:foreground ,foreground))))
   `(realgud-bp-disabled-face      ((,class (:foreground ,foreground))))
   `(realgud-bp-line-enabled-face  ((,class (:underline ,foreground))))
   `(realgud-bp-line-disabled-face ((,class (:underline ,foreground))))
   `(realgud-file-name             ((,class (:foreground ,foreground))))
   `(realgud-line-number           ((,class (:foreground ,foreground))))
   `(realgud-backtrace-number      ((,class (:foreground ,foreground :weight bold))))

   ;; outline
   `(outline-1 ((,class (:foreground ,foreground))))
   `(outline-2 ((,class (:foreground ,foreground))))
   `(outline-3 ((,class (:foreground ,foreground))))
   `(outline-4 ((,class (:foreground ,foreground))))
   `(outline-5 ((,class (:foreground ,foreground))))

   ;; org
   `(org-headline-done ((,class (:foreground ,foreground))))
   `(org-level-1 ((,class (:foreground ,foreground))))
   `(org-date ((,class (:foreground ,foreground))))
   `(org-done ((,class (:foreground ,foreground))))
   `(org-todo ((,class (:foreground ,foreground))))

   ;; avy
   `(avy-lead-face ((,class (:foreground ,foreground :background ,brightblack))))

   ;; ein
   `(ein:basecell-input-area-face ((,class (:background ,background))))

   ;; completions
   `(completions-annotations ((,class (:foreground ,brightwhite))))
   ))

(provide-theme 'hikizan)
;;; hikizan-theme.el ends here
