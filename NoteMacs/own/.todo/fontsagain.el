;(insert (prin1-to-string (x-list-fonts "*")))

;this builds a list of available fonts (in a special format) from the list of dirs in 'bdf-directory-list
;(describe-fontset)

(setq w32-bdf-filename-alist
      (w32-find-bdf-fonts bdf-directory-list))

  (create-fontset-from-fontset-spec
   "-*-fixed-medium-r-normal-*-16-*-*-*-c-*-fontset-bdf,
 japanese-jisx0208:-*-*-medium-r-normal-*-16-*-*-*-c-*-jisx0208.1983-*,
 katakana-jisx0201:-*-*-medium-r-normal-*-16-*-*-*-c-*-jisx0201*-*,
 latin-jisx0201:-*-*-medium-r-normal-*-16-*-*-*-c-*-jisx0201*-*,
 japanese-jisx0208-1978:-*-*-medium-r-normal-*-16-*-*-*-c-*-jisx0208.1978-*,
 thai-tis620:-misc-fixed-medium-r-normal--16-160-72-72-m-80-tis620.2529-1,
 lao:-misc-fixed-medium-r-normal--16-160-72-72-m-80-MuleLao-1,
 tibetan-1-column:-TibMdXA-fixed-medium-r-normal--16-160-72-72-m-80-MuleTibetan-1,
 ethiopic:-Admas-Ethiomx16f-Medium-R-Normal--16-150-100-100-M-160-Ethiopic-Unicode,
 tibetan:-TibMdXA-fixed-medium-r-normal--16-160-72-72-m-160-MuleTibetan-0")



(set-default-font "fontset-bdf")

(setq w32-use-w32-font-dialog nil)


(setq w32-fixed-font-alist
      (append w32-fixed-font-alist
              '(("intlfonts"
				 ("lao" "-*-fixed-medium-r-normal-*-24-*-mulelao-1")
				 ("tibetan" "-*-fixed-medium-r-normal-*-24-*-muletibetan-0")
				 ("tibetan-1-column" "-*-medium-r-normal-*-24-*-muletibetan-1")
				 ("thai-tis620" "-*-medium-r-normal-*-24-*-tis620.2529-*")
				 ("korean-ksc5601" "-*-mincho-medium-r-normal-*-24-*-ksc5601*-*")
				 ("japanese-jisx0212" "-fixed-medium-r-normal-*-24-*-jisx0212*-*")
				 ("japanese-jisx0208" "-fixed-medium-r-normal-*-24-*-jisx0208*-*")
				 ("latin-jisx0201" "-fixed-medium-r-normal-*-24-*-jisx0201*-*")
				 ("katakana-jisx0201" "-fixed-medium-r-normal-*-24-*-jisx0201*-*")
				 ("chinese-big5-1" "-*-fixed-medium-r-normal-*-24-*-big5.eten-0")
				 ("chinese-big5-2" "-*-fixed-medium-r-normal-*-24-*-big5.eten-0")
				 ("chinese-gb2312" "-*-medium-r-normal-*-24-*-gb2312*-*")
				 ("chinese-cns11643-1" "-*-medium-r-normal-*-24-*-cns11643*-1")
				 ("chinese-cns11643-2" "-*-medium-r-normal-*-24-*-cns11643*-2")
				 ("chinese-cns11643-3" "-*-medium-r-normal-*-24-*-cns11643*-3")
				 ("chinese-cns11643-4" "-*-medium-r-normal-*-24-*-cns11643*-4")
				 ("chinese-cns11643-5" "-*-medium-r-normal-*-24-*-cns11643*-5")
				 ("chinese-cns11643-6" "-*-medium-r-normal-*-24-*-cns11643*-6")
				 ("chinese-cns11643-7" "-*-medium-r-normal-*-24-*-cns11643*-")
))))
















;notes:
;(insert (prin1-to-string (x-list-fonts "*")))

;this builds a list of available fonts (in a special format) from the list of dirs in 'bdf-directory-list
;(describe-fontset)

(setq w32-bdf-filename-alist
      (w32-find-bdf-fonts bdf-directory-list))

(create-fontset-from-fontset-spec
 "-*-Courier New-normal-r-*-*-12-*-*-*-c-*-fontset-most,
 latin-iso8859-2:-*-Courier New-normal-r-*-*-12-*-*-*-c-*-iso8859-2,
 latin-iso8859-3:-*-Courier New-normal-r-*-*-12-*-*-*-c-*-iso8859-3,
 latin-iso8859-4:-*-Courier New-normal-r-*-*-12-*-*-*-c-*-iso8859-4,
 cyrillic-iso8859-5:-*-Courier New-normal-r-*-*-12-*-*-*-c-*-iso8859-5,
 greek-iso8859-7:-*-Courier New-normal-r-*-*-12-*-*-*-c-*-iso8859-7,
 latin-iso8859-9:-*-Courier New-normal-r-*-*-12-*-*-*-c-*-iso8859-9,
 latin-jisx0201:-*-*-medium-r-normal-*-16-*-*-*-c-*-jisx0201*-*,
 chinese-gb2312:-*-MS Song-normal-r-*-*-12-*-*-*-c-*-gb2312-*,
 chinese-big5-1:-*-MingLiU-normal-r-*-*-12-*-*-*-c-*-big5-*,
 chinese-big5-2:-*-MingLiU-normal-r-*-*-12-*-*-*-c-*-big5-*
 ethiopic:-Admas-Ethiomx16f-Medium-R-Normal--16-150-100-100-M-160-Ethiopic-Unicode,
 japanese-jisx0208:-*-*-medium-r-normal-*-16-*-*-*-c-*-jisx0208.1983-*,
 japanese-jisx0208-1978:-*-*-medium-r-normal-*-16-*-*-*-c-*-jisx0208.1978-*,
 katakana-jisx0201:-*-*-medium-r-normal-*-16-*-*-*-c-*-jisx0201*-*,
 lao:-misc-fixed-medium-r-normal--16-160-72-72-m-80-MuleLao-1,
 thai-tis620:-misc-fixed-medium-r-normal--16-160-72-72-m-80-tis620.2529-1,
 tibetan-1-column:-TibMdXA-fixed-medium-r-normal--16-160-72-72-m-80-MuleTibetan-1,
 tibetan:-TibMdXA-fixed-medium-r-normal--16-160-72-72-m-160-MuleTibetan-0"
 t t)

(create-fontset-from-fontset-spec
 "-*-fixed-medium-r-normal-*-16-*-*-*-c-*-fontset-bdf,
 japanese-jisx0208:-*-*-medium-r-normal-*-16-*-*-*-c-*-jisx0208.1983-*,
 katakana-jisx0201:-*-*-medium-r-normal-*-16-*-*-*-c-*-jisx0201*-*,
 latin-jisx0201:-*-*-medium-r-normal-*-16-*-*-*-c-*-jisx0201*-*,
 japanese-jisx0208-1978:-*-*-medium-r-normal-*-16-*-*-*-c-*-jisx0208.1978-*,
 thai-tis620:-misc-fixed-medium-r-normal--16-160-72-72-m-80-tis620.2529-1,
 lao:-misc-fixed-medium-r-normal--16-160-72-72-m-80-MuleLao-1,
 tibetan-1-column:-TibMdXA-fixed-medium-r-normal--16-160-72-72-m-80-MuleTibetan-1,
 ethiopic:-Admas-Ethiomx16f-Medium-R-Normal--16-150-100-100-M-160-Ethiopic-Unicode,
 tibetan:-TibMdXA-fixed-medium-r-normal--16-160-72-72-m-160-MuleTibetan-0" t t)

(create-fontset-from-fontset-spec
 "-*-fixed-medium-r-normal-*-14-*-*-*-c-*-fontset-bdf,
 japanese-jisx0208:-*-*-medium-r-normal-*-14-*-*-*-c-*-jisx0208.1983-*,
 katakana-jisx0201:-*-*-medium-r-normal-*-14-*-*-*-c-*-jisx0201*-*,
 latin-jisx0201:-*-*-medium-r-normal-*-14-*-*-*-c-*-jisx0201*-*,
 japanese-jisx0208-1978:-*-*-medium-r-normal-*-14-*-*-*-c-*-jisx0208.1978-*,
 thai-tis620:-misc-fixed-medium-r-normal--14-160-72-72-m-80-tis620.2529-1,
 lao:-misc-fixed-medium-r-normal--14-160-72-72-m-80-MuleLao-1,
 tibetan-1-column:-TibMdXA-fixed-medium-r-normal--14-160-72-72-m-80-MuleTibetan-1,
 ethiopic:-Admas-Ethiomx14f-Medium-R-Normal--14-150-100-100-M-160-Ethiopic-Unicode,
 tibetan:-TibMdXA-fixed-medium-r-normal--14-160-72-72-m-160-MuleTibetan-0")

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

(setq w32-use-w32-font-dialog nil)



;;;(set-default-font  "-*-Courier New-normal-r-*-*-12-90-96-96-c-*-iso8859-1") ;9

;;;(set-face-font 'italic "-*-Courier New-normal-i-*-*-11-*-*-*-c-*-iso8859-1")
;;;(set-face-font 'italic "-*-Courier New-normal-i-*-*-11-*-*-*-c-*-iso8859-1")
;;;(set-face-font 'bold-italic "-*-Courier New-bold-i-*-*-11-*-*-*-c-*-iso8859-1")

;;; (set-default-font  "-*-Courier New-normal-r-*-*-13-90-96-96-c-*-iso8859-1")
;;; (set-default-font "-*-Courier New-normal-r-*-*-14-*-*-*-c-*-*-ansi-")
;;; (set-default-font "-*-Courier-normal-r-*-*-16-106-*-*-c-90-*-ansi-")
;;; (set-default-font "-*-Fixedsys-normal-r-*-*-15-100-*-*-c-80-*-ansi-")
;;; (set-default-font "-*-Terminal-normal-r-*-*-12-80-*-*-c-80-*-oem-")
;;; (set-default-font "-*-Lucida Console-normal-r-*-*-11-82-96-96-c-*-iso8859-1")

