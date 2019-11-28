(require 'ox-latex)
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)

(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(setq exec-path (append exec-path '("/usr/bin/")))
(load "auctex.el" nil t t)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex) 

(org-babel-do-load-languages
 'org-babel-load-languages
 '((latex . t)))

(defun my-org-confirm-babel-evaluate (lang body)
  (not (string= lang "latex")))  ; don't ask for latex
(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)
