(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-biblatex-use-Biber nil t)
 '(LaTeX-command "xelatex")
 '(TeX-PDF-mode t)
 '(TeX-engine (quote xetex))
 '(TeX-source-correlate-method (quote synctex))
 '(TeX-source-correlate-mode t)
 '(TeX-source-correlate-start-server t)
 '(TeX-view-program-list
   (quote
    (("zathura" "zathura %o")
     ("okular" "okular --unique %o#src:%n%b"))))
 '(TeX-view-program-selection
   (quote
    ((engine-omega "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "zathura")
     (output-html "xdg-open"))))
 '(c-basic-offset 2)
 '(default-input-method "russian-computer")
 '(evil-shift-width 2)
 '(fci-rule-color "#073642")
 '(fill-column 80)
 '(haskell-font-lock-symbols t)
 '(haskell-tags-on-save t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(magit-diff-use-overlays nil)
 '(menu-bar-mode nil)
 '(org-export-latex-default-packages-alist
   (quote
    (("AUTO" "inputenc" t)
     ("T2A" "fontenc" t)
     ("" "fixltx2e" nil)
     ("" "graphicx" t)
     ("" "longtable" nil)
     ("" "float" nil)
     ("" "wrapfig" nil)
     ("" "soul" t)
     ("" "textcomp" t)
     ("" "marvosym" t)
     ("" "wasysym" t)
     ("" "latexsym" t)
     ("" "amssymb" t)
     ("" "hyperref" nil)
     ("russian,english" "babel" nil)
     "\\tolerance=1000")))
 '(org-latex-classes
   (quote
    (("beamer" "\\documentclass[presentation]{beamer}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("article" "\\documentclass[a4paper,11pt]{article}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("report" "\\documentclass[a4paper,11pt]{report}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("extarticle" "\\documentclass[a4paper,11pt]{extarticle}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("extreport" "\\documentclass[a4paper,11pt]{extreport}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("book" "\\documentclass[a4paper,11pt]{book}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))))
 '(org-latex-default-class "extarticle")
 '(org-latex-default-packages-alist
   (quote
    (("" "polyglossia" nil)
     ("" "amsmath" t)
     ("" "microtype" nil)
     ("" "unicode-math" t)
     ("" "graphicx" t)
     ("" "longtable" nil)
     ("" "float" nil)
     ("" "wrapfig" nil)
     ("" "rotating" nil)
     ("normalem" "ulem" t)
     ("" "hyperref" nil)
     "\\defaultfontfeatures{Mapping=tex-text}" "\\setmainfont{CMU Serif}" "\\setsansfont{CMU Sans Serif}" "\\setmonofont{CMU Typewriter Text}" "\\DeclareSymbolFont{letters}{\\encodingdefault}{\\rmdefault}{m}{it}")))
 '(org-latex-inputenc-alist nil)
 '(org-latex-pdf-process
   (quote
    ("xelatex -interaction nonstopmode -output-directory %o %f" "xelatex -interaction nonstopmode -output-directory %o %f" "xelatex -interaction nonstopmode -output-directory %o %f")))
 '(package-archives
   (quote
    (("mepla" . "https://melpa.org/packages/")
     ("marmalade" . "https://marmalade-repo.org/packages/")
     ("gnu" . "https://elpa.gnu.org/packages/"))))
 '(password-cache-expiry nil)
 '(preview-auto-cache-preamble t)
 '(python-indent-offset 2)
 '(rust-indent-offset 2)
 '(safe-local-variable-values
   (quote
    ((haskell-process-use-ghci . t)
     (haskell-indent-spaces . 2))))
 '(standard-indent 2)
 '(tab-stop-list
   (quote
    (2 4 6 8 10 12 14 16 18 20 22 24 26 28 30 32 34 36 38 40 42 44 46 48 50 52 54 56 58 60 62 64 66 68 70 72 74 76 78 80 82 84 86 88 90 92 94 96 98 100 102 104 106 108 110 112 114 116 118 120 122 124 126 128 130 132 134 136 138 140 142 144 146 148 150 152 154 156 158 160 162 164 166 168 170 172 174 176 178 180 182 184 186 188 190 192 194 196 198 200)))
 '(tab-width 2)
 '(tags-revert-without-query t)
 '(tool-bar-mode nil)
 '(vc-follow-symlinks t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro" :foundry "unknown" :slant normal :weight normal :height 98 :width normal)))))
