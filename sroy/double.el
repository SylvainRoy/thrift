;; Attempt to create elisp functions that encode/decode doubles.

(setq pow_8 (expt 2.0 8))
(setq pow_16 (expt 2.0 16))
(setq pow_24 (expt 2.0 24))
(setq pow_32 (expt 2.0 32))
(setq pow_40 (expt 2.0 40))
(setq pow_48 (expt 2.0 48))
(setq pow_52 (expt 2.0 52))
(setq pow_1022 (expt 2.0 1022))


(defun float-to-reverse-bin (fvalue)
  (let ((l ())
	tv
	(v fvalue))
    (while (/= v 0)
      (setq v (/ v 2))
      (setq tv (ftruncate v))
      (setq l (append l (list (if (= v tv) 0 1))))
      (setq v tv))
    l))

(defun value-of-least-n-significant-bits (n fvalue)
  (let ((bits (float-to-reverse-bin fvalue))
	(i 0)
	(vout 0)
	(exp 1))
    (while (and (< i n) bits)
      (setq vout (+ vout (* (pop bits) exp)))
      (setq exp (* 2 exp))
      (setq i (+ i 1)))
    vout))


(defun writeDouble (value)
  (let (m e c
	(buff [0 0 0 0 0 0 0 0])
	(v value))
    ;; sign
    (aset buff 0 (if (< v 0) #x80 #x00))
    (setq v (abs v))
    (cond
     ;; NaN
     ((isnan v)
      (progn
	(setq m 2251799813685248)
	(setq e 2047)))
     ;; Infinite
     ((equal v 1.0e+INF)
      (progn
	(setq m 0)
	(setq e 2047)))
     ;; standard number
     (t
      (progn
	(setq e (ffloor (/ (log v) (log 2))))
	(setq c (expt 2 (- e)))
	(when (< (* v c) 1)
	  (setq e (- e 1))
	  (setq c (* 2 c)))
	(cond
	 ((>= (+ e 1023) 2047)
	  (progn
	    (setq m 0)
	    (setq e 2047)))
	 ((>= (+ e 1023) 1)
	  (progn
	    (setq m (* (- (* v c) 1) pow_52))
	    (setq e (+ e 1023))))
	 (t
	  (progn
	    (setq m (* (* v pow_1022) pow_52))
	    (setq e 0)))))))

    (aset buff 1 (* 16 (value-of-least-n-significant-bits 4 e)))
    ;;           (logand (lsh e 4) #xF0))  ; <=> 16 * (e & #x0f)
    (aset buff 0 (logior (aref buff 0)
    ;;                   (logand (lsh e -4) #x7F)))
			 (value-of-least-n-significant-bits 7 (/ e 16))))

    (aset buff 7 (value-of-least-n-significant-bits 8 m))
    (setq m (/ m pow_8))
    (aset buff 6 (value-of-least-n-significant-bits 8 m))
    (setq m (/ m pow_8))
    (aset buff 5 (value-of-least-n-significant-bits 8 m))
    (setq m (/ m pow_8))
    (aset buff 4 (value-of-least-n-significant-bits 8 m))
    (setq m (/ m pow_8))
    (aset buff 3 (value-of-least-n-significant-bits 8 m))
    (setq m (/ m pow_8))
    (aset buff 2 (value-of-least-n-significant-bits 8 m))
    (setq m (/ m pow_8))
    (aset buff 1 (logior (aref buff 1)
			 (value-of-least-n-significant-bits 8 m)))
    buff))


;; Working OK:
(writeDouble 1.0)  ; OK: [63 240 0 0 0 0 0 0]
(writeDouble 1.1)  ; OK: [63 241 153 153 153 153 153 154]
(writeDouble -1.1) ; OK: [191 241 153 153 153 153 153 154]
(writeDouble 0.0)  ; OK: [0 0 0 0 0 0 0 0]


;; To investigate:

(writeDouble 1.0e+INF) ; KO, should be [127 240 0 0 0 0 0 0]
[126 224 0 0 0 0 0 0]

(writeDouble -1.0e+INF) ; KO, should be [255 240 0 0 0 0 0 0]
[254 224 0 0 0 0 0 0]

(writeDouble -0.000000001)  ; KO, but close. Expected [190 17 46 11 232 38 214 149]
[191 17 47 11 233 39 215 149]

(writeDouble 2398724098432.3478)  ; KO, but close. Expected [66 129 115 248 200 76 2 200]
[67 129 115 249 201 77 3 200]
