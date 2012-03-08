(setq bdf-directory-list
      '("~/intlfonts/Asian" "~/intlfonts/Chinese" "~/intlfonts/Chinese-X"
        "~/intlfonts/Ethiopic" "~/intlfonts/European" "~/intlfonts/Japanese"
        "~/intlfonts/Japanese-X" "~/intlfonts/Korean-X" "~/intlfonts/Misc"))
(setq w32-bdf-filename-alist
      (w32-find-bdf-fonts bdf-directory-list))

;Then create fontsets for the BDF fonts:
(create-fontset-from-fontset-spec
 "-*-fixed-medium-r-normal-*-24-*-*-*-c-*-fontset-bdf,
 japanese-jisx0208:-*-*-medium-r-normal-*-24-*-*-*-c-*-jisx0208.1983-*,
 katakana-jisx0201:-*-*-medium-r-normal-*-24-*-*-*-c-*-jisx0201*-*,
 latin-jisx0201:-*-*-medium-r-normal-*-24-*-*-*-c-*-jisx0201*-*,
 japanese-jisx0208-1978:-*-*-medium-r-normal-*-24-*-*-*-c-*-jisx0208.1978-*,
 thai-tis620:-misc-fixed-medium-r-normal--24-160-72-72-m-80-tis620.2529-1,
 lao:-misc-fixed-medium-r-normal--24-160-72-72-m-80-MuleLao-1,
 tibetan-1-column:-TibMdXA-fixed-medium-r-normal--24-160-72-72-m-80-MuleTibetan-1,
 ethiopic:-Admas-Ethiomx16f-Medium-R-Normal--24-150-100-100-M-160-Ethiopic-Unicode,
 tibetan:-TibMdXA-fixed-medium-r-normal--24-160-72-72-m-160-MuleTibetan-0")

;Many of the international bdf fonts from gnu.org are type 0, and therefore need to be added to font-encoding-alist:

;; Need to add some fonts to font-encoding-alist since the bdf fonts
;; are type 0 not the default type 1.
(setq font-encoding-alist
      (append '(("MuleTibetan-0" (tibetan . 0))
                ("GB2312"        (chinese-gb2312 . 0))
                ("JISX0208"      (japanese-jisx0208 . 0))
                ("JISX0212"      (japanese-jisx0212 . 0))
                ("VISCII"        (vietnamese-viscii-lower . 0))
                ("KSC5601"       (korean-ksc5601 . 0))
                ("MuleArabic-0"  (arabic-digit . 0))
                ("MuleArabic-1"  (arabic-1-column . 0))
                ("MuleArabic-2"  (arabic-2-column . 0))) font-encoding-alist))

;You can now use the Emacs font menu (not the popup dialog; see below ) to select the "bdf: 16-dot medium" fontset, or you can select it by setting the default font:

(set-default-font "fontset-bdf")

;Try loading the file etc/HELLO, and you should be able to see the various international fonts displayed (except for Hindi, which is not included in the 16-dot font distribution).
;How can I have Emacs use a font menu similar to the one it uses on Unix (with fonts and fontsets listed)?

;Place the following in your startup file:

(setq w32-use-w32-font-dialog nil)
(setq w32-use-w32-font-dialog t)

;The font I want to use is not in the font menu, how can I put it there?

;If you have set w32-use-w32-font-dialog to nil, you can add fonts to the font menu by changing `w32-fixed-font-alist'. For example:

(setq w32-fixed-font-alist
      (append w32-fixed-font-alist
              '(("Monotype.com"
   ("8" "-*-Monotype.com-normal-r-*-*-11-*-*-*-c-iso8859-1")
   ("9" "-*-Monotype.com-normal-r-*-*-12-*-*-*-c-iso8859-1")
   ("10" "-*-Monotype.com-normal-r-*-*-13-*-*-*-c-iso8859-1")
   ("11" "-*-Monotype.com-normal-r-*-*-15-*-*-*-c-iso8859-1")))))

(setq w32-fixed-font-alist
      (append w32-fixed-font-alist
              '(("intlfonts"
				 ("japanese-jisx0208" "-*-medium-r-normal-*-24-*-*-*-c-*-jisx0208.1983-*")
				 ("katakana-jisx0201" "-*-medium-r-normal-*-24-*-*-*-c-*-jisx0201*-*")
				 ("latin-jisx0201" "-*-medium-r-normal-*-24-*-*-*-c-*-jisx0201*-*")
				 ("japanese-jisx0208-1978" "-*-medium-r-normal-*-24-*-*-*-c-*-jisx0208.1978-*")
				 ("thai-tis620" "-misc-fixed-medium-r-normal--24-160-72-72-m-80-tis620.2529-1")
				 ("lao" "-misc-fixed-medium-r-normal--24-160-72-72-m-80-MuleLao-1")
				 ("tibetan-1-column" "-TibMdXA-fixed-medium-r-normal--24-160-72-72-m-80-MuleTibetan-1")
				 ("ethiopic" "-Admas-Ethiomx16f-Medium-R-Normal--24-150-100-100-M-160-Ethiopic-Unicode")

				 ("tibetan" "-TibMdXA-fixed-medium-r-normal--24-160-72-72-m-160-MuleTibetan-0")))))
