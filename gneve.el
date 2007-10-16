;;; gneve.el --- GNU Emacs video editor mode for editing video Edit Decision
;;; List or EDL

;; Copyright (C) 2007 Free Software Foundation, Inc.

;; Author: Martin Howse (m AT 1010 DOT co DOT uk)
;; Maintainer: Martin Howse (m AT 1010 DOT co DOT uk)
;; Committers: Arnold Matyasi (arn AT webma DOT hu)
;;             Gabor Torok (tgabor AT webma DOT hu)
;; URL: http://www.1010.co.uk/gneve.html
;; Compatibility: Emacs20, Emacs21, Emacs22, Emacs23

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Use:
;;     1. M-x gneve-start RET to invoke in gneve-mode
;;        C-x C-w to save *GNEVE* buffer as videname.edl for later usage
;;     2. C-x C-f any .edl file to open it in gneve-mode
;;     3. C-h m to visit gneve-mode help

;;; Install:

;; Put something similar to the following in your ~/.emacs to use this file:
;;
;; (add-to-list 'load-path "~/path/to/gneve-mode/")
;; (require 'gneve)

;;; Dependency:

;; gneve.el requires mplayer 1.0 and avidemux 2.4, lastest svn version recommended
;; mplayer:  patch time postition precision to 6 digits
;; -- checkout: svn checkout svn://svn.mplayerhq.hu/mplayer/trunk mplayer
;; -- patch:
;;--- command.c	(revision 23382)
;;+++ command.c	(working copy)
;;@@ -2325,7 +2325,7 @@
;; 		    pos =
;; 			playing_audio_pts(sh_audio, mpctx->d_audio,
;; 					  mpctx->audio_out);
;;-		mp_msg(MSGT_GLOBAL, MSGL_INFO, "ANS_TIME_POSITION=%.1f\n", pos);
;;+		mp_msg(MSGT_GLOBAL, MSGL_INFO, "ANS_TIME_POSITION=%.6f\n", pos);
;; 	    }
;; 	    break;
;; -- install: ./configure ; make ; su ; make install

;; avidemux:  use svn version
;; -- checkout: svn checkout svn://svn.berlios.de/avidemux/branches/avidemux_2.4_branch/
;; -- install: make -f Makefile.dist ; ./configure ; make ; su ; make install


;;; History:
;; 

;;; Code:

;; Keyboard shortcuts definition
(defvar gneve-mode-map
  (let ((gneve-mode-map (make-sparse-keymap)))
    ;; Video operation
    (define-key gneve-mode-map "V" 'gneve-open-film)
    (define-key gneve-mode-map "L" 'gneve-pause)
    (define-key gneve-mode-map "J" 'gneve-prev-frame)
    (define-key gneve-mode-map "K" 'gneve-next-frame)
    (define-key gneve-mode-map "Q" 'gneve-one-sec-back)
    (define-key gneve-mode-map "W" 'gneve-one-sec-forward)
    (define-key gneve-mode-map "A" 'gneve-five-sec-back)
    (define-key gneve-mode-map "S" 'gneve-five-sec-forward)
    ;; Mark operation
    (define-key gneve-mode-map "E" 'gneve-mark-start)
    (define-key gneve-mode-map "R" 'gneve-mark-end)
    (define-key gneve-mode-map "H" 'gneve-write-marks)
    (define-key gneve-mode-map "Z" 'gneve-goto-start)
    (define-key gneve-mode-map "X" 'gneve-goto-end)
    (define-key gneve-mode-map "C" 'gneve-goto-point)
    (define-key gneve-mode-map "G" 'gneve-goto-timecode)
    ;; Render operation
    (define-key gneve-mode-map "U" 'gneve-render-region)
    (define-key gneve-mode-map "I" 'gneve-render-buffer)
    (define-key gneve-mode-map "O" 'gneve-save-rendered)
    (define-key gneve-mode-map "P" 'gneve-play-rendered)
    (define-key gneve-mode-map "D" 'gneve-take-screenshot)
  gneve-mode-map)
  "Local keymap for GNEVE.")

(defconst gneve-number-regexp
  "-?\\([0-9]+\\.?\\|\\.\\)[0-9]*\\(e[0-9]+\\)?"
  "Regular expression for recognizing numbers.")

(defconst gneve-vslot-regexp
  "^\\([0-9]+\\):"
  "Regexp for video slot number.")

(defconst gneve-default-buffer "*GNEVE*" "Default gneve buffer.")

(defvar gneve-buffer gneve-default-buffer "GNEVE working buffer.")
(defvar vslots nil "Video slot file names list.")
(defvar vslot-n nil "Video slot number.")
(defvar vslot nil "Active video slot.")
;not used (defvar filename nil "Video filename.")
(defvar lastin nil "Lastin.")
(defvar lastout nil "Lastout.")
(defvar start nil "Start of timecode mark.")
(defvar end nil "End of timecode mark.")
(defvar videoname nil "Rendered videofile name.")
(defvar timecode-string nil "Timecode string.")
(defvar tc-hour nil "Timecode hour part.")
(defvar tc-min nil "Timecode minute part.")
(defvar tc-sec nil "Timecode second part.")
(defvar tc-msec nil "Timecode mili second part.")

;;;###autoload
(defun gneve-mode()
  "EDL and mplayer based GNU Emacs video editing mode

Video commands:
  V - Visit video file and start playing
  L - Pause/Play
  J - 1 frame back and pause
  K - 1 frame forward and pause
  Q - 1 second back and pause
  W - 1 second forward and pause
  A - 5 seconds back and pause
  S - 5 seconds forward and pause
  C - Goto timecode of current point
  G - Goto timecode of user input

  Layout summary:

  Q W
   A S     G H J K L
          V

Editing commands:
  E - Mark start of a section
  R - Mark end of a section
  H - Write marked section to EDL buffer
  Z - Goto start of marked section and pause
  X - Goto end of marked section and pause

  Layout summary:
      E R

    Z X C

Render commands:
  U - Render active region
  I - Render whole buffer
  O - Save rendered video
  P - Play rendered video

  Layout summary:
              U I O P"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'gneve-mode)
  (setq mode-name "gneve")
  (use-local-map gneve-mode-map))

(defun gneve-start ()
  "Create or reload a GNEVE session.
There are three cases:
1. If current buffer is a previously saved EDL buffer, then evaulate it
2. If current buffer is a newly created EDL buffer, then create base a
  structure
3. If there is no EDL buffer opened, then create a base structure"
  (interactive)
  (save-excursion
    ;; If there is no EDL buffer opened, open a temporary one
    (if (not (string-match "\\.edl" (buffer-name)))
        (pop-to-buffer gneve-default-buffer nil))
    (if (not (eq major-mode 'gneve-mode))
        (gneve-mode))
    (setq gneve-buffer (buffer-name))
    ;; If current buffer is a previously saved EDL buffer
    (goto-char (point-min))
    ;; If buffer contains valid vslots definition
    (if (looking-at-p "(setq vslots")
        ;; then evaluate it
        (and
         (eval-region 0 (search-forward "))" nil t))
         (forward-char 2))
      ;; else create base structure
      (insert "(setq vslots '( ))\n\n"))))

(defun gneve-vslot-pos (arg list)
  "Get video slot position.
Argument ARG video filename.
Argument LIST vslot list."
  (cond
   ((endp list) nil)
   ((equal arg (car list)) (length (cdr list)))
   (t (gneve-vslot-pos arg (cdr list)))))

(defun gneve-open-film (filename)
  "Open video file and create a vslot entry for it.
Argument FILENAME video filename."
  (interactive "ffilename:")
  (when (not (member (expand-file-name filename) vslots))
    (add-to-list 'vslots (expand-file-name filename) t)
    (switch-to-buffer gneve-buffer)
    (goto-char (point-min))
    (search-forward "))")
    (backward-char 2)
    (insert (format "\n\"%s\"" (expand-file-name filename)))
    (forward-char 2))
  (setq vslot-n (gneve-vslot-pos (expand-file-name filename) (reverse vslots)))
  (start-process "my-process" "boo" "mplayer" "-slave" "-vo" "x11" "-xy" "320" "-osdlevel" "1" "-quiet" (expand-file-name filename))
  (print (expand-file-name filename)))

(defun gneve-take-screenshot ()
  "Take screenshot of current frame."
  (interactive)
  (process-send-string "my-process" "screenshot 0\n"))

(defun gneve-next-frame ()
  "Seek next frame."
  (interactive)
  (process-send-string "my-process" "frame_step\n"))

(defun gneve-prev-frame ()
  "Seek previous frame."
  (interactive)
  (process-send-string "my-process" "seek -0.08\npause\n"))

(defun gneve-pause ()
  "Pause video play."
  (interactive)
  (process-send-string "my-process" "pause\n"))

(defun gneve-write-marks ()
  "Write video slot, lastin, lastout time code into EDL buffer."
  (interactive)
  (switch-to-buffer gneve-buffer)
  (insert (format "%d:%s %s\n" vslot-n lastin lastout)))

(defun gneve-one-sec-back ()
  "Seek one sec back."
  (interactive)
  (process-send-string "my-process" "seek -1.0\npause\n"))

(defun gneve-one-sec-forward ()
  "Seek one sec forward."
  (interactive)
  (process-send-string "my-process" "seek 1.0\npause\n"))

(defun gneve-five-sec-back ()
  "Seek five sec back."
  (interactive)
  (process-send-string "my-process" "seek -5.0\npause\n"))

(defun gneve-five-sec-forward ()
  "Seek five sec forward."
  (interactive)
  (process-send-string "my-process" "seek 5.0\npause\n"))

(defun gneve-mark-start ()
  "Mark start of a section."
  (interactive)
  (setq lastin (gneve-marker))
  (message "edit points %s %s\n" lastin lastout))

(defun gneve-mark-end ()
  "Mark end of a section."
  (interactive)
  (setq lastout (gneve-marker))
  (message "edit points %s %s\n" lastin lastout))

(defun gneve-marker ()
  ;; copies latest mark to boo. copy and paste only to variable - new function write to buffer
  (interactive)
  (progn
    ;; goto end of buffer search back to equals and copy to last-in
    (set-buffer "boo")
    (process-send-string "my-process" "pausing get_time_pos\n")
    (sleep-for 0.1)
    (goto-char (point-max))
    (backward-char)
    (backward-char)
    (setq end (point))
    (search-backward "=")
    (forward-char)
    (setq start (point))
    (copy-region-as-kill start end)
    (car kill-ring))) ;; stend

(defun gneve-goto-point ()
  (interactive)
  (setq timecode-string (read (current-buffer)))
  (process-send-string "my-process" (format "seek %s 2\npause\n" timecode-string)))

(defun gneve-tc-human ()
  "Calculate human readable timecode (hh:mm:ss,ms)."
  (setq tc-hour (/ (floor (string-to-number timecode-string)) 3600))
  (setq tc-min (/ (- (floor (string-to-number timecode-string)) (* 3600 tc-hour))  60))
  (setq tc-sec (- (string-to-number timecode-string) (* 60 tc-min) (* 3600 tc-hour)))
  (setq tc-msec (truncate (* 1000 (- tc-sec (floor tc-sec))))))


(defun gneve-goto-timecode ()
  "Goto user input timecode."
  (interactive)
  (defvar timecode-newpos nil "new timecode position string")
  (setq timecode-string (gneve-marker))
  (gneve-tc-human)
  (setq timecode-newpos  (read-string "Goto timecode (min:sec) " (format "%d:%.2f" tc-min tc-sec ) nil nil))
  (setq tc-min (car (split-string timecode-newpos ":")))
  (setq tc-sec (cadr (split-string timecode-newpos ":")))
  (setq timecode-string (number-to-string (+ (* 60 (string-to-number tc-min)) (string-to-number tc-sec))))
  (process-send-string "my-process" (format "seek %s 2\npause\n" timecode-string)))

(defun gneve-goto-start ()
  "Goto mark start."
  (interactive)
  (process-send-string "my-process" (format "seek %s 2\npause\n" lastin)))

(defun gneve-goto-end ()
  "Goto mark end."
  (interactive)
  (process-send-string "my-process" (format "seek %s 2\npause\n" lastout)))

(defun gneve-render-buffer ()
  "Render whole buffer."
  (interactive)
  (save-excursion
    (save-restriction
      (goto-char (point-min))
      (narrow-to-region (+ (search-forward  "))") 2) (point-max))
      (gneve-render)))
  (pop-to-buffer "avidemux"))

(defun gneve-render-region ()
  "Render only active region."
  (interactive)
  (save-excursion
    (save-restriction
      (narrow-to-region (region-beginning) (region-end))
      (gneve-render)))
  (pop-to-buffer "avidemux"))

(defun gneve-save-rendered ()
  "Save rendered video file."
  (interactive)
  (setq videoname (read-file-name "Save rendered video: " default-directory nil nil))
  (if (file-exists-p videoname)
  (or (y-or-n-p (format "Video `%s' already exists; overwrite? " videoname))
		 (error "Canceled")))
  (message videoname)
  (shell-command (concat "if [ -f \"/tmp/test.srt\" ]; then cp -f /tmp/test.srt " (expand-file-name videoname) ".srt; fi"))
  (shell-command (concat "time cp -f /tmp/test.avi " (expand-file-name videoname))))

(defun gneve-play-rendered ()
  "Play rendered video file."
  (interactive)
  (start-process "my-process3" nil "mplayer" "-vo" "x11" "-sub" "/tmp/test.srt" "-quiet" "/tmp/test.avi"))

(defun gneve-render()
  "Render EDL to Avidemux JS script."
  (interactive)
  (defvar startframe nil "start frame")
  (defvar endframe nil "end frame")
  (defvar subcounter 1 "subtitle counter")
  (defvar subtitle nil "subtitle string")
  (defvar lengthrendered 0 "length of rendered film")
  (setq subcounter 1)
  (setq lengthrendered 0)
  ;; count segments and write header with this info and source file
  (let ((old-point (point))
        (counter 0)
        (x 0))
    (goto-char (point-min))
    (while
        (re-search-forward "^[0-9]" nil t)
      (setq counter (1+ counter)))
    (goto-char old-point)
    (message "%d" counter)
    (switch-to-buffer (find-file-noselect "/tmp/test.srt"))
    (erase-buffer)
    (switch-to-buffer (find-file-noselect "/tmp/gnevetemp.js")) ;should be blank
    (erase-buffer)
    (insert "//AD\n//created by gneve.el\nvar app = new Avidemux();\n")
    (insert (format "app.load(\"%s\");\n" (car vslots)))
    (dolist (i (cdr vslots))
      (insert (format "app.append(\"%s\");\n" i)))
    (insert "app.clearSegments();\n")
    (switch-to-buffer gneve-buffer)
    (goto-char (point-min))
    (while (< x counter)
      (gneve-vslot-matcher)
      (setq startframe (gneve-timecode-matcher))
      (forward-char 1)
      (switch-to-buffer  "gnevetemp.js")
      (insert (format "app.addSegment(%s,%d," vslot startframe))
      (setq x (1+ x))
      (switch-to-buffer gneve-buffer)
      (setq endframe (gneve-timecode-matcher))
      ;; if there is subtitle string insert into srt buffer
      (unless (eolp)
	(looking-at ".+")
	(setq subtitle (match-string 0))
	(switch-to-buffer "test.srt")
	(insert (format "%d\n" subcounter))
	(setq timecode-string (number-to-string (/ lengthrendered 25)))
    (gneve-tc-human)
	(insert (format "%s:%s:%d,%s --> " tc-hour tc-min tc-sec tc-msec))
	(setq timecode-string (number-to-string (/ (+ lengthrendered (- endframe startframe)) 25)))
    (gneve-tc-human)
	(insert (format "%s:%s:%d,%s\n" tc-hour tc-min tc-sec tc-msec))
	(insert (format "%s\n\n" subtitle))
	(setq subcounter (1+ subcounter)))
      (switch-to-buffer  "gnevetemp.js")
      (insert (format "%d);\n" (- endframe startframe))) ;; length
      (setq lengthrendered (+ (- endframe startframe) lengthrendered))
      (switch-to-buffer gneve-buffer)
      (forward-line 1)
      (beginning-of-line))
    ;; write footer
    (progn
      (switch-to-buffer "gnevetemp.js")
      (goto-char (point-max))
      (insert (format "app.markerA=0;\napp.markerB=%d;\n" (- lengthrendered 1)))
      (insert (format "app.video.setPostProc(3,3,0);\napp.video.setFps1000(25000);app.video.codec(\"Copy\",\"CQ=4\",\"0\");app.audio.reset();app.audio.codec(\"copy\",128,0,\"\");app.audio.normalizeMode=0;app.audio.normalizeValue=0;app.audio.delay=0;app.audio.mixer(\"NONE\");app.setContainer(\"AVI\");setSuccess(app.save(\"/tmp/test.avi\"));"))
      ;; save and close temp file
      (save-buffer "/tmp/gnevetemp.js")
      (switch-to-buffer "test.srt")
      (save-buffer "/tmp/test.srt")
      ;; deal with avidemux
      (start-process "my-process2" "avidemux" "avidemux2_cli" "--run" "/tmp/gnevetemp.js" "--video-process" "--save" "/tmp/test.avi" "--quit")
      (kill-buffer "gnevetemp.js")
      (kill-buffer "test.srt")
      (switch-to-buffer gneve-buffer)
      (goto-char old-point))))

(defun gneve-timecode-matcher ()
  (let (timecode-string)
    (if (looking-at gneve-number-regexp)
	(goto-char (match-end 0)))
    (setq timecode-string (buffer-substring (match-beginning 0) (point)))
    (gneve-micros-to-frame (string-to-number timecode-string))))

(defun gneve-vslot-matcher ()
  (if (looking-at gneve-vslot-regexp)
      (goto-char (match-end 0)) )
  (setq vslot (buffer-substring (match-beginning 1) (- (point) 1))))

(defun gneve-micros-to-frame (microsec)
  "Convert microseconds to frame. 0.04 microsecond is one frame.
Argument MICROSEC microseconds."
  ;; 0.04 is one frame - divide by 0.04
  (/ microsec 0.04))

(defun gneve-time-to-frame (number)
  ;; min.sec convert to frame
  (/ number 0.04))

;;; List functions

(defalias 'first 'car)
(defalias 'second 'cadr)
(defalias 'rest 'cdr)
(defalias 'endp 'null)

(add-to-list 'auto-mode-alist '("\\.edl\\'" . gneve-mode))

(provide 'gneve)

;;; gneve.el ends here
