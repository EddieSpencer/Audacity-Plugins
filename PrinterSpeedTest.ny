;nyquist plug-in
;version 2
;type process
;categories "http://lv2plug.in/ns/lv2core#AnalyserPlugin"
;name "PrinterSpeedTest..."
;action "Finding sound..."
;info "Printer Speed Test Automation\n"
;control sil-lev "Treat audio below this level as silence [ -dB]" real "" 26 0 100
;control sil-dur "Minimum duration of silence between sounds [seconds]" real "" 0.03 0.01 5.0
;control labelbeforedur "Label starting point [seconds before sound starts]" real "" 0 0.0 1.0
;control labelafterdur "Label ending point [seconds after sound ends]" real "" 0 0.0 1.0
;control finallabel "Add a label at the end of the track? [No=0, Yes=1]" int "" 0 0 1
;control name "File name" string "" "Output.csv"

;;; Maximum of two channels if stereo
(defun mono-s (s-in) 
  (if (arrayp s-in)
      (s-max (aref s-in 0) (aref s-in 1))
      s-in))

;;; Low sample rate for faster processing
(defun my-s (s-in)
  (setq my-srate-ratio (truncate (/ (snd-srate (mono-s s-in)) 100)))
  (snd-avg (mono-s s-in) my-srate-ratio my-srate-ratio OP-PEAK))
  
;;; Add new items to the list of labels
(defun add-label (l-starttime l-endtime l-text)
  (setq labels-list
    (cons (list l-starttime l-endtime l-text) labels-list)))
	
;;; finds and determines home directory
(defun home ()
  (or (get-env "HOME")            ; Mac / Linux
      (get-env "UserProfile")))   ; Windows

;Set the silence threshold level (convert it to a linear form)
(setq thres (db-to-linear (* -1 sil-lev)))
;Store the sample rate of the sound
(setq s1-srate (snd-srate (my-s s)))
;Initialize the variable that will hold the length of the sound.
;Do not calculate it now with snd-length, because it would waste memory.
;We will calculate it later.
(setq s1-length 0)
;Initialize the silence counter
(setq sil-c 0)
;Initialize the labels variable
(setq labels-list NIL)
;Convert the silence duration in seconds to a length in samples
(setq sil-length (* sil-dur s1-srate))

;Set the sound-start marker to -1, indicating no sound has been found yet
(setq sound-start -1)
(setq silence-start -1)
;Set the flag that says we are looking for the start of a sound (as opposed to the start of a silence)
(setq sound-search 1)
;Set the counter that counts sounds
(setq sound-count 0)
(setq label-string "")

;; The main working part of the program, it counts
;; the number of sequential samples with volume under
;; the threshold. It adds to a list of markers every time
;; there is a longer period of silence than the silence
;; duration amount.

;; Loop and add to the list of markers (labels-list) each time it finds silence.
(let (s1) ;Define s1 as a local variable to allow efficient memory use
  ;; Get the sample into s1, then free s to save memory
  (setq s1 (my-s s))
  (setq s nil)
  ;; Capture the result of this "do" loop, because we need the sound's length
  ;; in samples.
  (setq s1-length
    ;; Keep repeating, incrementing the counter and getting another sample
    ;; each time through the loop.
    (do ((n 1 (+ n 1)) (v (snd-fetch s1) (setq v (snd-fetch s1))))
        ;; Exit when we run out of samples (v is nil) and return the number of
        ;; samples processed (n)
        ((not v) n)
      ;; Start the execution part of the do loop   
      ;; if found silence, increment the silence counter; if silence-start is 
      ;; not already > -1, set the start of silence to the current sample number (n)
      (if (< v thres) 
          (progn
            (setq sil-c (+ sil-c 1))
            (if (= silence-start -1) (setq silence-start n))))
     
      ;; if found sound, and sound-search is 1, mark the start of the sound and 
      ;; change sound-search to 0 (look for silence next)
      (if (and (>= v thres) (= sound-search 1))
          (progn
             (setq sound-search 0)
           (setq sound-start n)
           (setq sound-count (1+ sound-count))))

      ;; if found silence, and silence-counter is long enough, and sound-search is 0, 
      ;; and sound-start is not -1, and silence-start is not -1, that indicates 
      ;; the end of a sound (for which we have already found the beginning), 
      ;; which we should now label
      (if (and (< v thres) 
               (= sound-search 0)
               (/= sound-start -1)
               (> sil-c sil-length)
               (/= silence-start -1))
          (progn
            (setq sound-search 1)
            (setq sil-c 0)
            ;Create the label text
            (setq label-string (format nil "~A" (1- sound-count)))
            (add-label (- (/ sound-start s1-srate) labelbeforedur) (+ (/ silence-start s1-srate) labelafterdur) label-string)
            (setq label-string "")
            (setq silence-start -1)))
     
      ;; if found sound, reset the silence-counter and silence-start
      (if (>= v thres) 
          (progn
            (setq sil-c 0)
            (setq silence-start -1))))))
			
;; if we're still looking for the end of a sound at the end of the file, 
;; end the sound at the end of the file
(if (= sound-search 0)
    (progn
      (setq label-string (format nil "~A" (1- sound-count)))
      (add-label (- (/ sound-start s1-srate) labelbeforedur) (/ s1-length s1-srate) label-string)))

;; If no sound markers were found, return a message
;; Otherwise, if some sounds were found, also optionally 
;; place a label at the end of the file.
(if (null labels-list)
    (setq labels-list "No sounds found. Try reducing the silence\nlevel and minimum silence duration.")
    (if (= finallabel 1) (add-label (/ s1-length s1-srate) (/ s1-length s1-srate) "[End]")))

;;;;;;;;;; START OF NEW CODE ;;;;;;;;;
; file separator - the right sort of "slash" as a string.
(setq slash (format nil "~a" *file-separator*))

; remove the file separator from the end of the path
(setq path (string-right-trim slash (home)))

; put the path and file name together into
; one string and return to Audacity.
;;;Sets path of Output.csv to the UPSDATA, standard on all machines;;;;;;;;;;;;
(setq path "D:\\UPSDATA")
(setq filepath (format nil "~a~a~a" path slash name))

;open the file for writing
(setq file-pointer (open filepath :direction :output))

;;; START OF DATA WRITE ;;;
;; get number from string value
(defun string-to-num (string)
  (read (make-string-input-stream string)))

;; initialise global variables
(let ((odd-lab ())      ; list of odd labels
      (even-lab ())     ; list of even labels
      (even-tot 0)      ; total durations for even labels
      (starts-tot 0))   ; total durations for start of odd to start of even

  ;; the duration from the start of Beep to the start of Print
  ;; is the diference between the first item of the nth even label
  ;; and the first item of the nth odd label

  ; Get list in correct order and miss off first label
  (setf labels-list (cdr (reverse labels-list)))

  ;; extract data from labels-list
  (do* ((lnum 0 (1+ lnum))
        dur)
       ((= lnum (length labels-list)))
    (let* ((nth-label (nth lnum labels-list))
           (number (string-to-num (third nth-label))))
      (if (oddp number)
        (setf odd-lab (cons nth-label odd-lab))
        (progn
          (setf even-lab (cons nth-label even-lab))
          (setq dur (- (second nth-label)(first nth-label)))
          (setq even-tot (+ dur even-tot))))))
		  
  ;(print labels-list)
  (setf odd-lab (reverse odd-lab))
  (setf even-lab (reverse even-lab))
  (format file-pointer "Processing / TX (ms), Dispense Label (ms)\n")
  ;; print time from start of odd to start of even / duration of even
  (dotimes (i (length even-lab))
    (let* ((nth-odd (nth i odd-lab))                         ; next odd label
           (nth-even (nth i even-lab))                       ; next even label
           (starts-dur (- (first nth-even)(first nth-odd)))  ; time from odd start to even start
           (even-dur (- (second nth-even)(first nth-even)))) ; duration of even
      (format file-pointer  "~a,"
              (round (* 1000 starts-dur)))
      (format file-pointer  "~a~%"
              (round (* 1000 even-dur)))
      (setq starts-tot (+ starts-tot starts-dur))))

   ;; print average duration of even labels
  (format file-pointer  "~%Average Dispense Label (ms),~a~%"
          (round (* 1000 (/ even-tot (length even-lab)))))

  ;; print average duration of even labels
  (format file-pointer  "~%Average Processing / TX (ms),~a~%"
          (round (* 1000 (/ starts-tot (length even-lab)))))

  ;; print average duration of even labels
  (format file-pointer  "~%Total Average (ms),~a~%"
          (round (* 1000 (/ (+ starts-tot even-tot) (length even-lab))))))

; close the file
(close file-pointer)

; print a reminder to the debug window
; of where our file is written
(format t "The file should be here: ~%~a~%"
        filepath)

; return label list to Audacity
labels-list
