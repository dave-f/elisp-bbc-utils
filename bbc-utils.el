;;
;; BBC Utils
;; A collection of Emacs Lisp utility functions

;; This is a library for emacs which allows easy reading/writing of binary data etc, see https://github.com/rejeep/f.el
(require 'f)

;; This is a modern list library, see https://github.com/magnars/dash.el
(require 'dash)

(defconst pixelValues '(#b00000000 #b00000001 #b00000100 #b00000101 #b00010000 #b00010001 #b00010100 #b00010101
                        #b01000000 #b01000001 #b01000100 #b01000101 #b01010000 #b01010001 #b01010100 #b01010101))

(defconst pixelLeft  '#b10101010)
(defconst pixelRight '#b01010101)

(defun to-binary-string (i)
  "convert an integer into it's binary representation in string format"
  (let ((res ""))
    (while (not (= i 0))
      (setq res (concat (if (= 1 (logand i 1)) "1" "0") res))
      (setq i (lsh i -1)))
    (if (string= res "")
        (setq res "0"))
    res))

(defun split-number(arg)
  "split a 4 bit number to its 2 values"
  (concat (number-to-string (lsh arg -2)) "," (number-to-string (logand arg 3))))

(defun create-basic-nula-palette(filename)
  "create a buffer containing codes to set up the NuLA palette from basic"
  (interactive "fPalette file:")
  (switch-to-buffer (get-buffer-create "*palette*"))
  (erase-buffer)
  (let* ((file-bytes (string-to-list (f-read-bytes filename))))
    (message "length %d" (length file-bytes))
    (cl-loop for i from 0 to (1- (length file-bytes)) by 2 do
             (insert (format "?&FE23=&%02X : ?&FE23=&%02X\n" (nth i file-bytes) (nth (1+ i) file-bytes))))))

(defun decode-pixel (arg)
  "Given a number, returns the corresponding pixels for a Mode 2 byte"
  (interactive "nByte: ")
  (let* ((l (logand arg pixelLeft))
         (r (logand arg pixelRight))
         (pl (logior (logand (lsh l -1) #b1) (logand (lsh l -2) #b10) (logand (lsh l -3) #b100) (logand (lsh l -4) #b1000)))
         (pr (logior (logand r #b1) (logand (lsh r -1) #b10) (logand (lsh r -2) #b100) (logand (lsh r -3) #b1000))))
    (cons pl pr)))

(defun encode-pixel (left right)
  "Given two pixel colours, returns the corresponding Mode 2 byte"
  (logior (lsh (nth left pixelValues) 1) (nth right pixelValues)))