;;; codepage-ex.el --- codepage support extention

;; Copyright (C) 2004  ARISAWA Akihiro

;; Author: ARISAWA Akihiro <ari@mbf.sphere.ne.jp>
;; Keywords: languages, codepage

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; GNU Emacs support cp1250, cp1251, cp1253 and cp1257. But they don't
;; support characters in 0x80-0x9F.
;; This program support them, and provide cp1252, cp1254 and cp1255.

;; Here is a simple example of how to use this program.
;; (when (require 'codepage-ex nil t)
;;   (codepage-ex-setup "1252"))

;;; Code:

(require 'codepage)

(defconst codepage-ex-match-decode-table
  (let ((vec (make-vector 96 0))
	(i 0))
    (while (< i 96)
      (aset vec i (+ i 160))
      (setq i (1+ i)))
    vec))


(defvar cp1250-decode-table-for-eight-bit-control
  [ #x20AC ;; EURO SIGN
    nil ;; UNDEFINED
    #x201A ;; SINGLE LOW-9 QUOTATION MARK
    nil ;; UNDEFINED
    #x201E ;; DOUBLE LOW-9 QUOTATION MARK
    #x2026 ;; HORIZONTAL ELLIPSIS
    #x2020 ;; DAGGER
    #x2021 ;; DOUBLE DAGGER
    nil ;; UNDEFINED
    #x2030 ;; PER MILLE SIGN
    #x0160 ;; LATIN CAPITAL LETTER S WITH CARON
    #x2039 ;; SINGLE LEFT-POINTING ANGLE QUOTATION MARK
    #x015A ;; LATIN CAPITAL LETTER S WITH ACUTE
    #x0164 ;; LATIN CAPITAL LETTER T WITH CARON
    #x017D ;; LATIN CAPITAL LETTER Z WITH CARON
    #x0179 ;; LATIN CAPITAL LETTER Z WITH ACUTE
    nil ;; UNDEFINED
    #x2018 ;; LEFT SINGLE QUOTATION MARK
    #x2019 ;; RIGHT SINGLE QUOTATION MARK
    #x201C ;; LEFT DOUBLE QUOTATION MARK
    #x201D ;; RIGHT DOUBLE QUOTATION MARK
    #x2022 ;; BULLET
    #x2013 ;; EN DASH
    #x2014 ;; EM DASH
    nil ;; UNDEFINED
    #x2122 ;; TRADE MARK SIGN
    #x0161 ;; LATIN SMALL LETTER S WITH CARON
    #x203A ;; SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
    #x015B ;; LATIN SMALL LETTER S WITH ACUTE
    #x0165 ;; LATIN SMALL LETTER T WITH CARON
    #x017E ;; LATIN SMALL LETTER Z WITH CARON
    #x017A ;; LATIN SMALL LETTER Z WITH ACUTE
    ]
  "CP1250 (128..159) to UCS mapping")

(defvar cp1251-decode-table-for-eight-bit-control
  [ #x0402 ;; CYRILLIC CAPITAL LETTER DJE
    #x0403 ;; CYRILLIC CAPITAL LETTER GJE
    #x201A ;; SINGLE LOW-9 QUOTATION MARK
    #x0453 ;; CYRILLIC SMALL LETTER GJE
    #x201E ;; DOUBLE LOW-9 QUOTATION MARK
    #x2026 ;; HORIZONTAL ELLIPSIS
    #x2020 ;; DAGGER
    #x2021 ;; DOUBLE DAGGER
    #x20AC ;; EURO SIGN
    #x2030 ;; PER MILLE SIGN
    #x0409 ;; CYRILLIC CAPITAL LETTER LJE
    #x2039 ;; SINGLE LEFT-POINTING ANGLE QUOTATION MARK
    #x040A ;; CYRILLIC CAPITAL LETTER NJE
    #x040C ;; CYRILLIC CAPITAL LETTER KJE
    #x040B ;; CYRILLIC CAPITAL LETTER TSHE
    #x040F ;; CYRILLIC CAPITAL LETTER DZHE
    #x0452 ;; CYRILLIC SMALL LETTER DJE
    #x2018 ;; LEFT SINGLE QUOTATION MARK
    #x2019 ;; RIGHT SINGLE QUOTATION MARK
    #x201C ;; LEFT DOUBLE QUOTATION MARK
    #x201D ;; RIGHT DOUBLE QUOTATION MARK
    #x2022 ;; BULLET
    #x2013 ;; EN DASH
    #x2014 ;; EM DASH
    nil ;; UNDEFINED
    #x2122 ;; TRADE MARK SIGN
    #x0459 ;; CYRILLIC SMALL LETTER LJE
    #x203A ;; SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
    #x045A ;; CYRILLIC SMALL LETTER NJE
    #x045C ;; CYRILLIC SMALL LETTER KJE
    #x045B ;; CYRILLIC SMALL LETTER TSHE
    #x045F ;; CYRILLIC SMALL LETTER DZHE
    ]
  "CP1251 (128..159) to UCS mapping")

(defvar cp1252-decode-table codepage-ex-match-decode-table)
(setplist 'cp1252-decode-table
	  '(charset latin-iso8859-1 offset 160))
(defvar cp1252-decode-table-for-eight-bit-control
  [ #x20AC ;; 128:EURO SIGN
    nil	;; 129:UNDEFINED
    #x201A ;; 130:SINGLE LOW-9 QUOTATION MARK
    #x0192 ;; 131:LATIN SMALL LETTER F WITH HOOK
    #x201E ;; 132:DOUBLE LOW-9 QUOTATION MARK
    #x2026 ;; 133:HORIZONTAL ELLIPSIS
    #x2020 ;; 134:DAGGER
    #x2021 ;; 135:DOUBLE DAGGER
    #x02C6 ;; 136:MODIFIER LETTER CIRCUMFLEX ACCENT
    #x2030 ;; 137:PER MILLE SIGN
    #x0160 ;; 138:LATIN CAPITAL LETTER S WITH CARON
    #x2039 ;; 139:SINGLE LEFT-POINTING ANGLE QUOTATION MARK
    #x0152 ;; 140:LATIN CAPITAL LIGATURE OE
    nil	;; 141:UNDEFINED
    #x017D ;; 142:LATIN CAPITAL LETTER Z WITH CARON
    nil	;; 143:UNDEFINED
    nil	;; 144:UNDEFINED
    #x2018 ;; 145:LEFT SINGLE QUOTATION MARK
    #x2019 ;; 146:RIGHT SINGLE QUOTATION MARK
    #x201C ;; 147:LEFT DOUBLE QUOTATION MARK
    #x201D ;; 148:RIGHT DOUBLE QUOTATION MARK
    #x2022 ;; 149:BULLET
    #x2013 ;; 150:EN DASH
    #x2014 ;; 151:EM DASH
    #x02DC ;; 152:SMALL TILDE
    #x2122 ;; 153:TRADE MARK SIGN
    #x0161 ;; 154:LATIN SMALL LETTER S WITH CARON
    #x203A ;; 155:SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
    #x0153 ;; 156:LATIN SMALL LIGATURE OE
    nil	;; 157:UNDEFINED
    #x017E ;; 158:LATIN SMALL LETTER Z WITH CARON
    #x0178 ;; 159:LATIN CAPITAL LETTER Y WITH DIAERESIS
    ]
  "CP1252 (128..159) to UCS mapping")

(defvar cp1253-decode-table-for-eight-bit-control
  [ #x20AC ;; EURO SIGN
    nil ;; UNDEFINED
    #x201A ;; SINGLE LOW-9 QUOTATION MARK
    #x0192 ;; LATIN SMALL LETTER F WITH HOOK
    #x201E ;; DOUBLE LOW-9 QUOTATION MARK
    #x2026 ;; HORIZONTAL ELLIPSIS
    #x2020 ;; DAGGER
    #x2021 ;; DOUBLE DAGGER
    nil ;; UNDEFINED
    #x2030 ;; PER MILLE SIGN
    nil ;; UNDEFINED
    #x2039 ;; SINGLE LEFT-POINTING ANGLE QUOTATION MARK
    nil ;; UNDEFINED
    nil ;; UNDEFINED
    nil ;; UNDEFINED
    nil ;; UNDEFINED
    nil ;; UNDEFINED
    #x2018 ;; LEFT SINGLE QUOTATION MARK
    #x2019 ;; RIGHT SINGLE QUOTATION MARK
    #x201C ;; LEFT DOUBLE QUOTATION MARK
    #x201D ;; RIGHT DOUBLE QUOTATION MARK
    #x2022 ;; BULLET
    #x2013 ;; EN DASH
    #x2014 ;; EM DASH
    nil ;; UNDEFINED
    #x2122 ;; TRADE MARK SIGN
    nil ;; UNDEFINED
    #x203A ;; SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
    nil ;; UNDEFINED
    nil ;; UNDEFINED
    nil ;; UNDEFINED
    nil ;; UNDEFINED
    ]
  "CP1253 (128..159) to UCS mapping")

(defvar cp1254-decode-table codepage-ex-match-decode-table)
(setplist 'cp1254-decode-table
	  '(charset latin-iso8859-9 offset 160))
(defvar cp1254-decode-table-for-eight-bit-control
  [ #x20AC ;; EURO SIGN
    nil ;; UNDEFINED
    #x201A ;; SINGLE LOW-9 QUOTATION MARK
    #x0192 ;; LATIN SMALL LETTER F WITH HOOK
    #x201E ;; DOUBLE LOW-9 QUOTATION MARK
    #x2026 ;; HORIZONTAL ELLIPSIS
    #x2020 ;; DAGGER
    #x2021 ;; DOUBLE DAGGER
    #x02C6 ;; MODIFIER LETTER CIRCUMFLEX ACCENT
    #x2030 ;; PER MILLE SIGN
    #x0160 ;; LATIN CAPITAL LETTER S WITH CARON
    #x2039 ;; SINGLE LEFT-POINTING ANGLE QUOTATION MARK
    #x0152 ;; LATIN CAPITAL LIGATURE OE
    nil ;; UNDEFINED
    nil ;; UNDEFINED
    nil ;; UNDEFINED
    nil ;; UNDEFINED
    #x2018 ;; LEFT SINGLE QUOTATION MARK
    #x2019 ;; RIGHT SINGLE QUOTATION MARK
    #x201C ;; LEFT DOUBLE QUOTATION MARK
    #x201D ;; RIGHT DOUBLE QUOTATION MARK
    #x2022 ;; BULLET
    #x2013 ;; EN DASH
    #x2014 ;; EM DASH
    #x02DC ;; SMALL TILDE
    #x2122 ;; TRADE MARK SIGN
    #x0161 ;; LATIN SMALL LETTER S WITH CARON
    #x203A ;; SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
    #x0153 ;; LATIN SMALL LIGATURE OE
    nil ;; UNDEFINED
    nil ;; UNDEFINED
    #x0178 ;; LATIN CAPITAL LETTER Y WITH DIAERESIS
    ]
  "CP1254 (128..159) to UCS mapping")

(defvar cp1255-decode-table codepage-ex-match-decode-table)
(setplist 'cp1255-decode-table
	  '(charset hebrew-iso8859-8 offset 160))
(defvar cp1255-decode-table-for-eight-bit-control
  [ #x20AC ;; EURO SIGN
    nil ;; UNDEFINED
    #x201A ;; SINGLE LOW-9 QUOTATION MARK
    #x0192 ;; LATIN SMALL LETTER F WITH HOOK
    #x201E ;; DOUBLE LOW-9 QUOTATION MARK
    #x2026 ;; HORIZONTAL ELLIPSIS
    #x2020 ;; DAGGER
    #x2021 ;; DOUBLE DAGGER
    #x02C6 ;; MODIFIER LETTER CIRCUMFLEX ACCENT
    #x2030 ;; PER MILLE SIGN
    nil ;; UNDEFINED
    #x2039 ;; SINGLE LEFT-POINTING ANGLE QUOTATION MARK
    nil ;; UNDEFINED
    nil ;; UNDEFINED
    nil ;; UNDEFINED
    nil ;; UNDEFINED
    nil ;; UNDEFINED
    #x2018 ;; LEFT SINGLE QUOTATION MARK
    #x2019 ;; RIGHT SINGLE QUOTATION MARK
    #x201C ;; LEFT DOUBLE QUOTATION MARK
    #x201D ;; RIGHT DOUBLE QUOTATION MARK
    #x2022 ;; BULLET
    #x2013 ;; EN DASH
    #x2014 ;; EM DASH
    #x02DC ;; SMALL TILDE
    #x2122 ;; TRADE MARK SIGN
    nil ;; UNDEFINED
    #x203A ;; SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
    nil ;; UNDEFINED
    nil ;; UNDEFINED
    nil ;; UNDEFINED
    nil ;; UNDEFINED
    ]
  "CP1255 (128..159) to UCS mapping")

(defvar cp1257-decode-table-for-eight-bit-control
  [ #x20AC ;; EURO SIGN
    nil ;; UNDEFINED
    #x201A ;; SINGLE LOW-9 QUOTATION MARK
    nil ;; UNDEFINED
    #x201E ;; DOUBLE LOW-9 QUOTATION MARK
    #x2026 ;; HORIZONTAL ELLIPSIS
    #x2020 ;; DAGGER
    #x2021 ;; DOUBLE DAGGER
    nil ;; UNDEFINED
    #x2030 ;; PER MILLE SIGN
    nil ;; UNDEFINED
    #x2039 ;; SINGLE LEFT-POINTING ANGLE QUOTATION MARK
    nil ;; UNDEFINED
    #x00A8 ;; DIAERESIS
    #x02C7 ;; CARON
    #x00B8 ;; CEDILLA
    nil ;; UNDEFINED
    #x2018 ;; LEFT SINGLE QUOTATION MARK
    #x2019 ;; RIGHT SINGLE QUOTATION MARK
    #x201C ;; LEFT DOUBLE QUOTATION MARK
    #x201D ;; RIGHT DOUBLE QUOTATION MARK
    #x2022 ;; BULLET
    #x2013 ;; EN DASH
    #x2014 ;; EM DASH
    nil ;; UNDEFINED
    #x2122 ;; TRADE MARK SIGN
    nil ;; UNDEFINED
    #x203A ;; SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
    nil ;; UNDEFINED
    #x00AF ;; MACRON
    #x02DB ;; OGONEK
    nil ;; UNDEFINED
    ]
  "CP1257 (128..159) to UCS mapping")

(defun cp-ex-coding-system-for-codepage-1 (coding mnemonic iso-name
					       decoder encoder)
  "Make coding system CODING for a DOS codepage using translation tables.
MNEMONIC is a character to be displayed on mode line for the coding system.
ISO-NAME is the name of the ISO-8859 charset which corresponds to this
codepage.
DECODER is a translation table for converting characters in the DOS codepage
encoding to Emacs multibyte characters.
ENCODER is a translation table for encoding Emacs multibyte characters into
external DOS codepage codes."
  (save-match-data
    (let* ((coding-name (symbol-name coding))
	   (undef (if (eq system-type 'ms-dos)
		      (if dos-unsupported-char-glyph
			  (logand dos-unsupported-char-glyph 255)
			127)
		    ??))
	   (safe-chars (make-char-table 'safe-chars))
	   (ccl-decoder
	    (ccl-compile
	     ;; The 4 here supplies the buf_magnification parameter
	     ;; for the CCL program.  A multibyte character may take
	     ;; at most 4-bytes.
	     `(4 (loop (read r1)
		       (if (r1 >= 128)
			   ((r0 = ,(charset-id 'ascii))
			    (translate-character ,decoder r0 r1)
			    (write-multibyte-character r0 r1))
			 (write r1))
		       (repeat)))))
	   (ccl-encoder
	    (ccl-compile
	     ;; The 2 here supplies the buf_magnification parameter for
	     ;; the CCL program.  Since the -dos coding system generates
	     ;; \r\n for each \n, a factor of 2 covers even the worst case
	     ;; of empty lines with a single \n.
	     `(2 (loop (read-multibyte-character r0 r1)
		       (if (r0 != ,(charset-id 'ascii))
			   ((translate-character ,encoder r0 r1)
			    (if (r0 == ,(charset-id 'japanese-jisx0208))
				((r1 = ,undef)
				 (write r1)))))
		       (write-repeat r1))))))

      ;; Set elements of safe multibyte characters for this codepage
      ;; to t in the char-table safe-chars.
      (let ((tbl (get decoder 'translation-table))
	    (i 128)
	    ch)
	(while (< i 256)
	  (setq ch (aref tbl i))
	  (if ch (aset safe-chars ch t))
	  (setq i (1+ i))))

      ;; Make coding system CODING.
      (make-coding-system
       coding 4 mnemonic
       (concat "8-bit encoding of " (symbol-name iso-name)
	       " characters using IBM codepage " coding-name)
       (cons ccl-decoder ccl-encoder)
       `((safe-chars . ,safe-chars)
	 (valid-codes (0 . 255)))))))

(defun cp-ex-decoding-vector-for-codepage
  (table charset offset decode-table-for-eight-bit-control)
  "Create a vector for decoding IBM PC characters using conversion table
TABLE into an ISO-8859 character set CHARSET whose first non-ASCII
character is generated by (make-char CHARSET OFFSET)."
  (let* ((len (length table))
	 (undefined-char
	  (if (eq system-type 'ms-dos)
	      (if dos-unsupported-char-glyph
		  (logand dos-unsupported-char-glyph 255)
		127)
	    32))
	 (vec1 (make-vector 256 undefined-char))
	 (i 0))
    (while (< i 256)
      (aset vec1 i i)
      (setq i (1+ i)))
    (setq i 0)
    (while (< i len)
      (if (aref table i)
	  (aset vec1 (aref table i) (make-char charset (+ i offset))))
      (setq i (1+ i)))
    (setq i 128)
    (while (< i 160)
      (when (aref decode-table-for-eight-bit-control (- i 128))
	(aset vec1 i
	      (decode-char 'ucs (aref decode-table-for-eight-bit-control
				      (- i 128)))))
      (setq i (1+ i)))
    vec1))

(defun cp-ex-make-coding-systems-for-codepage (codepage iso-name offset)
  "Create a coding system to convert IBM CODEPAGE into charset ISO-NAME
whose first character is at offset OFFSET from the beginning of 8-bit
ASCII table.

The created coding system has the usual 3 subsidiary systems: for Unix-,
DOS- and Mac-style EOL conversion.  However, unlike built-in coding
systems, the Mac-style EOL conversion is currently not supported by the
decoder and encoder created by this function."
  (let* ((decode-table (intern (format "%s-decode-table" codepage)))
	 (decode-table-for-eight-bit-control
	  (intern (format "%s-decode-table-for-eight-bit-control" codepage)))
	 (nonascii-table
	  (intern (format "%s-nonascii-translation-table" codepage)))
	 (decode-translation
	  (intern (format "%s-decode-translation-table" codepage)))
	 (encode-translation
	  (intern (format "%s-encode-translation-table" codepage))))
    (set nonascii-table
	 (make-translation-table-from-vector
	  (cp-ex-decoding-vector-for-codepage
	   (symbol-value decode-table) iso-name offset
	   (symbol-value decode-table-for-eight-bit-control))))
    (define-translation-table encode-translation
      (char-table-extra-slot (symbol-value nonascii-table) 0))
    (define-translation-table decode-translation
      (symbol-value nonascii-table))
    (cp-coding-system-for-codepage-1
     (intern codepage) ?D iso-name decode-translation encode-translation)
    ))

(defun cp-ex-supported-codepages ()
  "Return an alist of supported codepages.

Each association in the alist has the form (NNN . CHARSET), where NNN is the
codepage number, and CHARSET is the MULE charset which is the closest match
for the character set supported by that codepage.

A codepage NNN is supported if a variable called `cpNNN-decode-table' exists,
is a vector, and has a charset property."
  (save-match-data
    (let (alist chset sname)
      (mapatoms
       (function
	(lambda (sym)
	  (if (and (boundp sym)
		   (string-match "\\`cp\\([1-9][0-9][0-9][0-9]?\\)-decode-table-for-eight-bit-control\\'"
				 (setq sname (symbol-name sym)))
		   (vectorp (symbol-value sym)))
	      (setq alist
		    (cons (cons (match-string 1 sname) chset) alist))))))
      alist)))

(defun codepage-ex-setup (codepage)
  "Create a coding system cpCODEPAGE to support the IBM codepage CODEPAGE.

These coding systems are meant for encoding and decoding 8-bit non-ASCII
characters used by the IBM codepages, typically in conjunction with files
read/written by MS-DOS software, or for display on the MS-DOS terminal."
  (interactive
   (let ((completion-ignore-case t)
	 (candidates (cp-ex-supported-codepages)))
     (list (completing-read "Setup DOS Codepage: (default 1252) " candidates
			    nil t nil nil "1252"))))
  (let ((cp (format "cp%s" codepage)))
    (cp-ex-make-coding-systems-for-codepage
     cp (cp-charset-for-codepage cp) (cp-offset-for-codepage cp))))

(provide 'codepage-ex)
;;; codepage-ex.el ends here
