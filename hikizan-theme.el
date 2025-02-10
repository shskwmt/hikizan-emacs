;;;###theme-autoload
(deftheme hikizan
  :background-mode 'dark
  :kind 'color-scheme)

(let ((class '((class color) (min-colors 89)))
      (default "default")
      (black "black")
      (blue "blue")
      (skyblue "SkyBlue4")
      (red "red")
      (white "white")
      (brightwhite "brightwhite")
      (brightblack "brightblack")
      (brightcyan "brightcyan")
      )
  (custom-theme-set-faces
   'hikizan
   `(default ((,class (:foreground ,default :background ,default))))

   ;; Highlighting faces
   `(fringe ((,class (:background ,brightblack))))
   `(highlight ((,class (:foreground ,default :background ,brightblack))))
   `(region ((,class (:background ,brightblack))))
   `(secondary-selection ((,class (:background ,brightblack))))
   `(isearch ((,class (:foreground ,default :background ,brightblack))))
   `(lazy-highlight ((,class (:background ,brightblack))))
   `(trailing-whitespace ((,class (:background ,brightblack))))

   ;; Mode line faces
   `(mode-line ((,class
   		 (:box (:line-width -1 :style released-button)
   		  :background ,brightblack :foreground ,default))))
   `(mode-line-inactive ((,class
   			  (:box (:line-width -1 :style released-button)
   			   :background ,default :foreground ,default))))
   `(compilation-mode-line-fail ((,class (:foreground ,default))))
   `(compilation-mode-line-run  ((,class (:foreground ,default))))
   `(compilation-mode-line-exit ((,class (:foreground ,default))))

   ;; Escape and prompt faces
   `(minibuffer-prompt ((,class (:foreground ,default))))
   `(escape-glyph ((,class (:foreground ,default))))
   `(homoglyph ((,class (:foreground ,default))))
   `(error ((,class (:foreground ,default))))
   `(warning ((,class (:foreground ,default))))
   `(success ((,class (:foreground ,default))))

   ;; Font lock faces
   `(font-lock-builtin-face ((,class (:foreground ,skyblue))))
   `(font-lock-comment-face ((,class (:foreground ,brightwhite))))
   `(font-lock-constant-face ((,class (:foreground ,skyblue))))
   `(font-lock-function-name-face ((,class (:foreground ,skyblue))))
   `(font-lock-keyword-face ((,class (:foreground ,skyblue))))
   `(font-lock-string-face ((,class (:foreground ,default))))
   `(font-lock-type-face ((,class (:foreground ,skyblue))))
   `(font-lock-variable-name-face ((,class (:foreground ,skyblue))))

   ;; Button and link faces
   `(link ((,class (:underline t :foreground ,default))))
   `(link-visited ((,class (:underline t :foreground ,default))))

   ;; Message faces
   `(message-header-name ((,class (:foreground ,default))))
   `(message-header-cc ((,class (:foreground ,default))))
   `(message-header-other ((,class (:foreground ,default))))
   `(message-header-subject ((,class (:foreground ,default))))
   `(message-header-to ((,class (:foreground ,default))))
   `(message-cited-text ((,class (:foreground ,default))))
   `(message-separator ((,class (:foreground ,default))))

   ;; SMerge faces
   `(smerge-refined-change ((,class (:background ,default))))

   ;; Flyspell faces
   `(flyspell-duplicate ((,class (:underline ,default))))
   `(flyspell-incorrect ((,class (:underline ,default))))

   ;; Realgud
   `(realgud-overlay-arrow1  ((,class (:foreground ,default))))
   `(realgud-overlay-arrow2  ((,class (:foreground ,default))))
   `(realgud-overlay-arrow3  ((,class (:foreground ,default))))
   `(realgud-bp-disabled-face      ((,class (:foreground ,default))))
   `(realgud-bp-line-enabled-face  ((,class (:underline ,default))))
   `(realgud-bp-line-disabled-face ((,class (:underline ,default))))
   `(realgud-file-name             ((,class (:foreground ,default))))
   `(realgud-line-number           ((,class (:foreground ,default))))
   `(realgud-backtrace-number      ((,class (:foreground ,default :weight bold))))

   ;; Semantic faces
   `(semantic-decoration-on-includes ((,class (:underline ,default))))
   `(semantic-decoration-on-private-members-face
     ((,class (:background ,default))))
   `(semantic-decoration-on-protected-members-face
     ((,class (:background ,default))))
   `(semantic-decoration-on-unknown-includes
     ((,class (:background ,default))))
   `(semantic-decoration-on-unparsed-includes
     ((,class (:background ,default))))
   `(semantic-tag-boundary-face ((,class (:overline ,default))))
   `(semantic-unmatched-syntax-face ((,class (:underline ,default))))

   ;; outline
   `(outline-1 ((,class (:foreground ,default))))
   `(outline-2 ((,class (:foreground ,default))))
   `(outline-3 ((,class (:foreground ,default))))
   `(outline-4 ((,class (:foreground ,default))))
   `(outline-5 ((,class (:foreground ,default))))

   ;; org
   `(org-headline-done ((,class (:foreground ,default))))
   `(org-level-1 ((,class (:foreground ,default))))
   `(org-date ((,class (:foreground ,default))))
   `(org-done ((,class (:foreground ,default))))
   `(org-todo ((,class (:foreground ,default))))

   ;; avy
   `(avy-lead-face ((,class (:foreground ,default :background ,brightblack))))

   ;; magit
   `(magit-section ((,class (:foreground ,default :background ,default))))
   `(magit-section-highlight ((,class (:foreground ,default :background ,default))))
   `(magit-diff-file-heading-selection ((,class (:foreground ,default))))
   `(magit-diff-hunk-heading ((,class (:foreground ,default :background ,default))))
   `(magit-diff-hunk-heading-selectgion ((,class (:foreground ,default :background ,brightblack))))
   `(magit-diff-hunk-heading-highlight ((,class (:foreground ,default :background ,default))))
   `(magit-diff-base-highlight ((,class (:foreground ,default :background ,default))))
   `(magit-diff-context-highlight ((,class (:foreground ,default :background ,default))))
   `(magit-diff-added ((,class (:foreground ,blue :background ,default))))
   `(magit-diff-added-highlight ((,class (:foreground ,blue :background ,default))))
   `(magit-diff-removed ((,class (:foreground ,red :background ,default))))
   `(magit-diff-removed-highlight ((,class (:foreground ,red :background ,default))))
   ))

(provide-theme 'hikizan)
