;;;###theme-autoload
(deftheme hikizan
  :background-mode 'dark
  :kind 'color-scheme)

(let ((class '((class color) (min-colors 89)))
      (black "#000000")
      (blue "blue")
      (red "red")
      (white "#ffffff")
      (brightblack "brightblack")
      (brightcyan "brightcyan")
      )
  (custom-theme-set-faces
   'hikizan
   `(default ((,class (:foreground ,white :background ,black))))
   `(cursor ((,class (:background ,black))))
   `(header-line ((,class (:background ,black))))

   ;; Highlighting faces
   `(fringe ((,class (:background ,brightblack))))
   `(highlight ((,class (:foreground ,white :background ,brightblack))))
   `(region ((,class (:background ,brightblack))))
   `(secondary-selection ((,class (:background ,brightblack))))
   `(isearch ((,class (:foreground ,white :background ,brightblack))))
   `(lazy-highlight ((,class (:background ,brightblack))))
   `(trailing-whitespace ((,class (:background ,brightblack))))

   ;; Mode line faces
   `(mode-line ((,class
		 (:box (:line-width -1 :style released-button)
		  :background ,white :foreground ,black))))
   `(mode-line-inactive ((,class
			  (:box (:line-width -1 :style released-button)
			   :background ,brightblack :foreground ,black))))
   `(compilation-mode-line-fail ((,class (:foreground ,black))))
   `(compilation-mode-line-run  ((,class (:foreground ,black))))
   `(compilation-mode-line-exit ((,class (:foreground ,black))))

   ;; Escape and prompt faces
   `(minibuffer-prompt ((,class (:foreground ,white))))
   `(escape-glyph ((,class (:foreground ,white))))
   `(homoglyph ((,class (:foreground ,white))))
   `(error ((,class (:foreground ,white))))
   `(warning ((,class (:foreground ,white))))
   `(success ((,class (:foreground ,white))))

   ;; Font lock faces
   `(font-lock-builtin-face ((,class (:foreground ,white))))
   `(font-lock-comment-face ((,class (:foreground ,white))))
   `(font-lock-constant-face ((,class (:foreground ,white))))
   `(font-lock-function-name-face ((,class (:foreground ,white))))
   `(font-lock-keyword-face ((,class (:foreground ,white))))
   `(font-lock-string-face ((,class (:foreground ,white))))
   `(font-lock-type-face ((,class (:foreground ,white))))
   `(font-lock-variable-name-face ((,class (:foreground ,white))))

   ;; Button and link faces
   `(link ((,class (:underline t :foreground ,white))))
   `(link-visited ((,class (:underline t :foreground ,white))))

   ;; Gnus faces
   `(gnus-group-news-1 ((,class (:foreground ,white))))
   `(gnus-group-news-1-low ((,class (:foreground ,white))))
   `(gnus-group-news-2 ((,class (:foreground ,white))))
   `(gnus-group-news-2-low ((,class (:foreground ,white))))
   `(gnus-group-news-3 ((,class (:foreground ,white))))
   `(gnus-group-news-3-low ((,class (:foreground ,white))))
   `(gnus-group-news-4 ((,class (:foreground ,white))))
   `(gnus-group-news-4-low ((,class (:foreground ,white))))
   `(gnus-group-news-5 ((,class (:foreground ,white))))
   `(gnus-group-news-5-low ((,class (:foreground ,white))))
   `(gnus-group-news-low ((,class (:foreground ,white))))
   `(gnus-group-mail-1 ((,class (:foreground ,white))))
   `(gnus-group-mail-1-low ((,class (:foreground ,white))))
   `(gnus-group-mail-2 ((,class (:foreground ,white))))
   `(gnus-group-mail-2-low ((,class (:foreground ,white))))
   `(gnus-group-mail-3 ((,class (:foreground ,white))))
   `(gnus-group-mail-3-low ((,class (:foreground ,white))))
   `(gnus-group-mail-low ((,class (:foreground ,white))))
   `(gnus-header-content ((,class (:weight normal :foreground ,white))))
   `(gnus-header-from ((,class (:foreground ,white))))
   `(gnus-header-subject ((,class (:foreground ,white))))
   `(gnus-header-name ((,class (:foreground ,white))))
   `(gnus-header-newsgroups ((,class (:foreground ,white))))

   ;; Message faces
   `(message-header-name ((,class (:foreground ,white))))
   `(message-header-cc ((,class (:foreground ,white))))
   `(message-header-other ((,class (:foreground ,white))))
   `(message-header-subject ((,class (:foreground ,white))))
   `(message-header-to ((,class (:foreground ,white))))
   `(message-cited-text ((,class (:foreground ,white))))
   `(message-separator ((,class (:foreground ,white))))

   ;; SMerge faces
   `(smerge-refined-change ((,class (:background ,black))))

   ;; Ediff faces
   `(ediff-current-diff-A ((,class (:background ,black))))
   `(ediff-fine-diff-A ((,class (:background ,black))))
   `(ediff-even-diff-A ((,class (:background ,black))))
   `(ediff-odd-diff-A ((,class (:background ,black))))
   `(ediff-current-diff-B ((,class (:background ,black))))
   `(ediff-fine-diff-B ((,class (:background ,black))))
   `(ediff-even-diff-B ((,class (:background ,black))))
   `(ediff-odd-diff-B ((,class (:background ,black))))

   ;; Flyspell faces
   `(flyspell-duplicate ((,class (:underline ,white))))
   `(flyspell-incorrect ((,class (:underline ,white))))

   ;; Realgud
   `(realgud-overlay-arrow1  ((,class (:foreground ,white))))
   `(realgud-overlay-arrow2  ((,class (:foreground ,white))))
   `(realgud-overlay-arrow3  ((,class (:foreground ,white))))
   `(realgud-bp-disabled-face      ((,class (:foreground ,white))))
   `(realgud-bp-line-enabled-face  ((,class (:underline ,white))))
   `(realgud-bp-line-disabled-face ((,class (:underline ,white))))
   `(realgud-file-name             ((,class (:foreground ,white))))
   `(realgud-line-number           ((,class (:foreground ,white))))
   `(realgud-backtrace-number      ((,class (:foreground ,white :weight bold))))

   ;; Semantic faces
   `(semantic-decoration-on-includes ((,class (:underline ,white))))
   `(semantic-decoration-on-private-members-face
     ((,class (:background ,black))))
   `(semantic-decoration-on-protected-members-face
     ((,class (:background ,black))))
   `(semantic-decoration-on-unknown-includes
     ((,class (:background ,black))))
   `(semantic-decoration-on-unparsed-includes
     ((,class (:background ,black))))
   `(semantic-tag-boundary-face ((,class (:overline ,white))))
   `(semantic-unmatched-syntax-face ((,class (:underline ,white))))

   ;; outline
   `(outline-1 ((,class (:foreground ,white))))
   `(outline-2 ((,class (:foreground ,white))))
   `(outline-3 ((,class (:foreground ,white))))
   `(outline-4 ((,class (:foreground ,white))))
   `(outline-5 ((,class (:foreground ,white))))

   ;; org
   `(org-headline-done ((,class (:foreground ,white))))
   `(org-level-1 ((,class (:foreground ,white))))

   ;; avy
   `(avy-lead-face ((,class (:foreground ,black :background ,brightcyan))))

   ;; magit
   `(magit-section ((,class (:foreground ,white :background ,black))))
   `(magit-diff-file-heading-selection ((,class (:foreground ,white))))
   `(magit-diff-hunk-heading ((,class (:foreground ,white :background ,black))))
   `(magit-diff-hunk-heading-selectgion ((,class (:foreground ,white :background ,brightblack))))
   `(magit-diff-hunk-heading-highlight ((,class (:foreground ,white :background ,black))))
   `(magit-diff-base-highlight ((,class (:foreground ,white :background ,black))))
   `(magit-diff-context-highlight ((,class (:foreground ,white :background ,black))))
   `(magit-diff-added ((,class (:foreground ,blue :background ,black))))
   `(magit-diff-added-highlight ((,class (:foreground ,blue :background ,black))))
   `(magit-diff-removed ((,class (:foreground ,red :background ,black))))
   `(magit-diff-removed-highlight ((,class (:foreground ,red :background ,black))))
   ))

(provide-theme 'hikizan)
