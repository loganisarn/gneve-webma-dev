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
;;     1. M-x `gneve-start' RET to invoke in `gneve-mode'
;;        C-x C-w to save *GNEVE* buffer as videoname.edl for latter usage
;;     2. C-x C-f any .edl file to open it in `gneve-mode'
;;     3. C-h m to visit `gneve-mode' help

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

;;; Bugs/todo:

;; - For parsing EDLs use registers instead of regex search
;; - Support other video players e.g. VLC
;; - Support customized running of Mplayer
;; - In function `gneve-tc-human' use locale variables to avoid the need of four
;; global bindings: `tc-hour', `tc-min', `tc-sec', `tc-msec'
;; - Add prefix to global variables

;;; History:
;; 

;;; Code:

(defgroup gneve nil
  "*GNU Emacs Video Editor mode for editing video Edit Decision List or EDL."
  :link '(url-link "http://www.1010.co.uk/gneve.html")
  :prefix "erc-"
  :group 'multimedia
  :group 'applications)

(defcustom gneve-tmp-dir "/tmp/gneve"
  "*GNEVE main temporary directory.  No trailing slash."
  :type 'directory
  :group 'gneve)

(defcustom gneve-thumb-dir (concat gneve-tmp-dir "/thumbs")
  "*Cache directory of image thumbnails.  No trailing slash."
  :type 'directory
  :group 'gneve)

(defcustom gneve-wav-dir (concat gneve-tmp-dir "/wav")
  "*Cache directory of wave audio files.  No trailing slash."
  :type 'directory
  :group 'gneve)

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
    (define-key gneve-mode-map "T" 'gneve-insert-timeline)

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

(defconst gneve-default-buffer "*GNEVE*"
  "Default GNEVE buffer.")

(defconst gneve-datacode-end ";;GNEVE datacode ends here"
  "Emacs Lisp comment in EDL buffer: the end of file definition part and the start of timecode part.")

(defvar gneve-buffer gneve-default-buffer
  "GNEVE working buffer.")
(make-variable-buffer-local 'gneve-buffer)

(defvar gneve-video-process nil
  "GNEVE video process ID.")
(make-variable-buffer-local 'gneve-video-process)

(defvar gneve-video-process-buffer nil
  "GNEVE video process buffer name.")
(make-variable-buffer-local 'gneve-video-process-buffer)

(defvar gneve-vslots nil
  "Video slot file names list.")
(make-variable-buffer-local 'gneve-vslots)

(defvar gneve-vslot-n 0
  "Video slot number.")
(make-variable-buffer-local 'gneve-vslot-n)

(defvar gneve-aslots nil
  "Audio slot file names list.")
(make-variable-buffer-local 'gneve-aslots)

(defvar gneve-mark-lastin 0
  "Start of marked section.")
(make-variable-buffer-local 'gneve-mark-lastin)

(defvar gneve-mark-lastout 0
  "End of marked section.")
(make-variable-buffer-local 'gneve-mark-lastout)

(defvar timecode-string nil
  "Timecode string.")

(defvar tc-hour nil
  "Timecode hour part.")

(defvar tc-min nil
  "Timecode minute part.")

(defvar tc-sec nil
  "Timecode second part.")

(defvar tc-msec nil
  "Timecode mili second part.")

(defvar gneve-timeline-matrix
  '((0.04 0.08 0.12 0.16 0.2)
    (1 2 3 4 5)
    (5 10 15 20 25))
  "Timeline definitions for 1 frame, 1 sec, 5 sec.")

(defvar gneve-timeline-step 0
  "Range of timeline.")

;;;###autoload
(defun gneve-mode ()
  "EDL and mplayer based GNU Emacs video editing mode.

Video commands:
  V - Visit video file and start playing
  G - Goto timecode of user input
  C - Goto timecode of current point
  L - Play/pause
  J - 1 frame back and pause
  K - 1 frame forward and pause
  Q - 1 second back and pause
  W - 1 second forward and pause
  A - 5 seconds back and pause
  S - 5 seconds forward and pause

  Layout summary:
  Q W
   A S     G   J K L
        C V

Editing commands:
  E - Mark start of a section
  R - Mark end of a section
  H - Write marked section to EDL buffer
  Z - Goto start of marked section and pause
  X - Goto end of marked section and pause
  T - Write timeline

  Layout summary:
      E R T
            H
    Z X

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
  (setq mode-name "GNEVE")
  (use-local-map gneve-mode-map)
  (add-hook 'after-save-hook 'gneve-write-file nil t)
  (gneve-init)
  (run-hooks 'gneve-mode-hook))

(defun gneve-init ()
  "Initialize a GNEVE session."
  ;; Set defaults
  (setq gneve-buffer (buffer-name)
        gneve-vslots nil
        gneve-aslots nil)
  ;; Temporary directory creating
  (if (not (file-exists-p gneve-tmp-dir))
      (make-directory gneve-tmp-dir t))
  (if (not (file-exists-p gneve-thumb-dir))
      (make-directory gneve-thumb-dir))
  (if (not (file-exists-p gneve-wav-dir))
      (make-directory gneve-wav-dir))
  ;; Parsing EDL buffer content
  (save-excursion
    (goto-char (point-min))
    ;; If datacode end marker not found, insert one
    (unless (search-forward gneve-datacode-end nil t)
      ;; Compatibility layer for old fashioned EDLs
      (re-search-forward gneve-vslot-regexp nil t)
      (beginning-of-line 1)
      (insert gneve-datacode-end "\n"))
    (search-backward gneve-datacode-end nil t)
    (save-restriction
      (narrow-to-region (point-min) (point))
      (goto-char (point-min))
      ;; Evaluate vslots definition
      (forward-list 1)
      (setq gneve-vslots (eval-last-sexp nil))
      ;; Evaluate aslots definition
      (forward-list 1)
      (when (and (not (eobp)) (not (looking-at gneve-datacode-end)))
        (setq gneve-aslots (eval-last-sexp nil))))))

(defun gneve-start ()
  "Create a new GNEVE session."
  (interactive)
  (pop-to-buffer gneve-default-buffer nil)
  (if (not (eq major-mode 'gneve-mode))
      (gneve-mode)
    (gneve-init)))

(defun gneve-write-file ()
  "Update variable `gneve-buffer' on buffer visiting file change."
  (if (not (string-equal (buffer-name) gneve-buffer))
      (setq gneve-buffer (buffer-name)))
  ;; Return nil to not to break hook flow
  nil)

(defun gneve-vslot-pos (vslot list)
  "Get position of VSLOT in LIST."
  (cond
   ((null list) nil)
   ((equal vslot (car list)) (length (cdr list)))
   (t (gneve-vslot-pos vslot (cdr list)))))

(defun gneve-insert-datacode ()
  "Insert vslots and aslots definition blocks."
  (save-excursion
    (save-restriction
      (search-forward gneve-datacode-end nil t)
      (narrow-to-region (point-min) (search-backward gneve-datacode-end nil t))
      (goto-char (point-min))
      (kill-sexp 1)
      (if gneve-vslots
          (insert (format "'%S\n" gneve-vslots)))
      (kill-sexp 1)
      (if gneve-aslots
          (insert (format "'%S\n" gneve-aslots))))))

(defun gneve-open-film (filename render-buffer)
  "Open video file FILENAME in RENDER-BUFFER and create a vslot entry for it."
  (interactive
   (list (car (find-file-read-args "Find video file: " t))
         (buffer-name)))
  (if (not (file-regular-p filename))
      (error "%s is not a video file" filename))
  ;; If video file is not already in a slot
  (when (not (member (expand-file-name filename) gneve-vslots))
    (add-to-list 'gneve-vslots (expand-file-name filename) t)
    (switch-to-buffer render-buffer)
    (gneve-insert-datacode))
  ;; Lookup video in vslots
  (setq gneve-vslot-n
        (gneve-vslot-pos (expand-file-name filename) (reverse gneve-vslots))
        ;; Initialize markers
        gneve-mark-lastin 0
        gneve-mark-lastout 0)
  ;; Start video process
  (let ((video-process (concat "gneve-video-process-" render-buffer)))
    ;; Set process ID and process buffer
    (setq gneve-video-process-buffer (generate-new-buffer-name video-process)
          ;; Function `start-process' automatically generates new process ID
          ;; when specified one exists
          gneve-video-process (process-name (start-process video-process gneve-video-process-buffer "mplayer" "-slave" "-vo" "x11" "-vf" "scale" "-zoom" "-xy" "320" "-osdlevel" "1" "-quiet" (expand-file-name filename)))))
  (message (format "Now playing: %s" (expand-file-name filename))))

(defun gneve-take-screenshot ()
  "Take screenshot of current frame."
  (interactive)
  (process-send-string gneve-video-process "screenshot 0\n"))

(defun gneve-insert-timeline ()
  "Insert timeline according to editing context:
1. If timecode line start end
2. If newline according to forward speed: 5 frame, 1 sec and 5 sec timeline"
  ;; Decide context, timecode line or current point and speed
  (interactive)
  (setq timecode-string (gneve-marker))
  (switch-to-buffer gneve-buffer)
  ;; Make thumbnail cache directory for current previewing video
  (let ((thumbfiles) (thumbfile) (thumb-position)
        (thumb-process (concat "gneve-create-thumbs" gneve-buffer))
        (thumbdir (concat gneve-thumb-dir "/" (md5 (nth gneve-vslot-n gneve-vslots)))))
    (if (not (file-exists-p thumbdir))
        (make-directory thumbdir t))
    ;; Make thumbfiles if not exists
    (dolist (i (nth gneve-timeline-step gneve-timeline-matrix))
      (setq thumb-position
            (+ i (string-to-number timecode-string))
            thumbfile (format "thumb-%g.png" thumb-position))
      (add-to-list 'thumbfiles thumbfile t)
      (if (not (file-exists-p (concat thumbdir "/" thumbfile)))
          (start-process thumb-process nil "ffmpeg" "-v" "0" "-y" "-ss" (number-to-string thumb-position) "-i" (nth gneve-vslot-n gneve-vslots) "-vcodec" "png" "-vframes" "1" "-an" "-f" "rawvideo" "-s" "80x60" (concat thumbdir "/" thumbfile))))
    (gneve-tc-human)
    (insert
     (format "\n%s:%s %.5f\n" gneve-vslot-n timecode-string
             (+ (car (last (nth gneve-timeline-step gneve-timeline-matrix)))
                (string-to-number timecode-string))))
    (sleep-for 0.1)
    (dolist (j thumbfiles)
      (insert-image (create-image (concat thumbdir "/" j)) nil nil nil))
    (insert "\n")))

(defun gneve-next-frame ()
  "Seek next frame."
  (interactive)
  (setq gneve-timeline-step 0)
  (process-send-string gneve-video-process "frame_step\n"))

(defun gneve-prev-frame ()
  "Seek previous frame."
  (interactive)
  (setq gneve-timeline-step 0)
  (process-send-string gneve-video-process "seek -0.08\npause\n"))

(defun gneve-pause ()
  "Pause video play."
  (interactive)
  (process-send-string gneve-video-process "pause\n"))

(defun gneve-write-marks ()
  "Write video slot, lastin, lastout time code into EDL buffer."
  (interactive)
  (switch-to-buffer gneve-buffer)
  (insert
   (format "%d:%s %s\n" gneve-vslot-n gneve-mark-lastin gneve-mark-lastout)))

(defun gneve-one-sec-back ()
  "Seek one sec back."
  (interactive)
  (setq gneve-timeline-step 1)
  (process-send-string gneve-video-process "seek -1.0\npause\n"))

(defun gneve-one-sec-forward ()
  "Seek one sec forward."
  (interactive)
  (setq gneve-timeline-step 1)
  (process-send-string gneve-video-process "seek 1.0\npause\n"))

(defun gneve-five-sec-back ()
  "Seek five sec back."
  (interactive)
  (setq gneve-timeline-step 2)
  (process-send-string gneve-video-process "seek -5.0\npause\n"))

(defun gneve-five-sec-forward ()
  "Seek five sec forward."
  (interactive)
  (setq gneve-timeline-step 2)
  (process-send-string gneve-video-process "seek 5.0\npause\n"))

(defun gneve-mark-start ()
  "Mark start of a section."
  (interactive)
  (setq gneve-mark-lastin (gneve-marker))
  (message "Edit points: %s %s" gneve-mark-lastin gneve-mark-lastout))

(defun gneve-mark-end ()
  "Mark end of a section."
  (interactive)
  (setq gneve-mark-lastout (gneve-marker))
  (message "Edit points: %s %s" gneve-mark-lastin gneve-mark-lastout))

(defun gneve-marker ()
  "Read timecode values from `gneve-video-process-buffer'."
  ;; Copy latest mark to `gneve-video-process-buffer'.
  ;; Copy and paste only to variable - new function write to buffer.
  ;; Goto end of buffer search back to equals and copy to last-in.
  (save-excursion
    (set-buffer gneve-video-process-buffer)
    (process-send-string gneve-video-process "pausing get_time_pos\n")
    (sleep-for 0.1)
    (goto-char (point-max))
    (backward-char 2)
    (let ((end (point)))
      (search-backward "=")
      (forward-char)
      (copy-region-as-kill (point) end)
      (car kill-ring))))

(defun gneve-goto-point ()
  "Seek timecode."
  (interactive)
  (setq timecode-string (read (current-buffer)))
  (process-send-string gneve-video-process (format "seek %s 2\npause\n" timecode-string)))

(defun gneve-tc-human ()
  "Calculate human readable timecode (hh:mm:ss,ms)."
  (setq tc-hour (/ (floor (string-to-number timecode-string)) 3600))
  (setq tc-min (/ (- (floor (string-to-number timecode-string)) (* 3600 tc-hour))  60))
  (setq tc-sec (- (string-to-number timecode-string) (* 60 tc-min) (* 3600 tc-hour)))
  (setq tc-msec (truncate (* 1000 (- tc-sec (floor tc-sec))))))

(defun gneve-goto-timecode ()
  "Goto user input timecode."
  (interactive)
    (let (timecode-newpos) ; new timecode position string
      (setq timecode-string (gneve-marker))
      (gneve-tc-human)
      (setq timecode-newpos
            (read-string "Goto timecode (min:sec) "
                         (format "%d:%.2f" tc-min tc-sec) nil nil))
      (setq tc-min (car (split-string timecode-newpos ":")))
      (setq tc-sec (cadr (split-string timecode-newpos ":")))
      (setq timecode-string (number-to-string (+ (* 60 (string-to-number tc-min)) (string-to-number tc-sec))))
      (process-send-string gneve-video-process (format "seek %s 2\npause\n" timecode-string))))

(defun gneve-goto-start ()
  "Goto mark start."
  (interactive)
  (process-send-string gneve-video-process (format "seek %s 2\npause\n" gneve-mark-lastin)))

(defun gneve-goto-end ()
  "Goto mark end."
  (interactive)
  (process-send-string gneve-video-process (format "seek %s 2\npause\n" gneve-mark-lastout)))

(defun gneve-render-buffer (render-buffer)
  "Render whole RENDER-BUFFER."
  (interactive (list (buffer-name)))
  (save-excursion
    (save-restriction
      (goto-char (point-min))
      (re-search-forward gneve-vslot-regexp nil t)
      (beginning-of-line 1)
      (narrow-to-region (point) (point-max))
      (gneve-render-video render-buffer)))
  (pop-to-buffer (concat "gneve-render-process-" render-buffer)))

(defun gneve-render-region (render-buffer)
  "Render active region in RENDER-BUFFER."
  (interactive (list (buffer-name)))
  (save-excursion
    (save-restriction
      (narrow-to-region (region-beginning) (region-end))
      (gneve-render-video render-buffer)))
  (pop-to-buffer (concat "gneve-render-process-" render-buffer)))

(defun gneve-save-rendered (videoname render-buffer)
  "Save rendered video file as VIDEONAME."
  (interactive
   (list
    (read-file-name "Save rendered video file: " default-directory nil nil)
    (buffer-name)))
  (if (file-exists-p videoname)
      (or (y-or-n-p (format "Video `%s' already exists; overwrite? " videoname))
          (error "Canceled")))
  (message videoname)
  (shell-command (format "if [ -f \"%s/%s.srt\" ]; then cp -f %s/%s.srt %s.srt; fi" gneve-tmp-dir render-buffer gneve-tmp-dir render-buffer (expand-file-name videoname)))
  (shell-command (format "time cp -f %s/%s.avi %s" gneve-tmp-dir render-buffer (expand-file-name videoname))))

(defun gneve-play-rendered (render-buffer)
  "Play rendered video file."
  (interactive (list (buffer-name)))
  (let ((srt (concat gneve-tmp-dir "/" render-buffer ".srt"))
        (avi (concat gneve-tmp-dir "/" render-buffer ".avi"))
        (playback-process (concat "gneve-playback-process-" render-buffer)))
    (start-process playback-process nil "mplayer" "-vo" "x11" "-sub" srt "-quiet" avi)))

(defun gneve-render-video (render-buffer)
  "Render EDL in RENDER-BUFFER to Avidemux JS script."
  ;; Count segments and write header with this info and source file
  (let* ((subcounter 1) (lengthrendered 0) (old-point (point)) (counter 0)
         (srt (concat render-buffer ".srt"))
         (js (concat render-buffer ".js"))
         (avi (concat render-buffer ".avi"))
         (srt-path (concat gneve-tmp-dir "/" srt))
         (js-path (concat gneve-tmp-dir "/" js))
         (avi-path (concat gneve-tmp-dir "/" avi))
         (render-process (concat "gneve-render-process-" render-buffer))
         startframe endframe subtitle vslot my-vslots)
    ;; `vslots' is buffer local variable, in future we should use the proper
    ;; mode prefix to avoid this "my-vslots" hack
    (set-buffer render-buffer)
    (setq my-vslots gneve-vslots)
    (goto-char (point-min))
    ;; Count timecode strings
    (while (re-search-forward "^[0-9]" nil t)
      (setq counter (1+ counter)))
    (goto-char old-point)
    (message "Number of timecode strings: %d" counter)
    (switch-to-buffer (find-file-noselect srt-path))
    (erase-buffer)
    (switch-to-buffer (find-file-noselect js-path))
    (erase-buffer)
    (insert "//AD\n//created by gneve.el\nvar app = new Avidemux();\n")
    (insert (format "app.load(\"%s\");\n" (car my-vslots)))
    (dolist (i (cdr my-vslots))
      (insert (format "app.append(\"%s\");\n" i)))
    (insert "app.clearSegments();\n")
    (switch-to-buffer render-buffer)
    (goto-char (point-min))
    (dotimes (i counter)
      (setq vslot (gneve-vslot-match))
      (setq startframe (gneve-timecode-match))
      (forward-char 1)
      (switch-to-buffer js)
      (insert (format "app.addSegment(%s,%d," vslot startframe))
      (switch-to-buffer render-buffer)
      (setq endframe (gneve-timecode-match))
      ;; If there is subtitle string, insert into srt buffer
      (unless (eolp)
        (looking-at ".+")
        (setq subtitle (match-string 0))
        ;; TODO: handling @ orig audio fade , # video section fade , % flags audio and video section fade
        (switch-to-buffer srt)
        (insert (format "%d\n" subcounter))
        (setq timecode-string (number-to-string (/ lengthrendered 25)))
        (gneve-tc-human)
        (insert (format "%s:%s:%d,%s --> " tc-hour tc-min tc-sec tc-msec))
        (setq timecode-string (number-to-string (/ (+ lengthrendered (- endframe startframe)) 25)))
        (gneve-tc-human)
        (insert (format "%s:%s:%d,%s\n" tc-hour tc-min tc-sec tc-msec))
        (insert (format "%s\n\n" subtitle))
        (setq subcounter (1+ subcounter)))
      (switch-to-buffer js)
      (insert (format "%d);\n" (- endframe startframe))) ; length
      (setq lengthrendered (+ (- endframe startframe) lengthrendered))
      (switch-to-buffer render-buffer)
      (forward-line 1)
      (beginning-of-line))
    ;; Write footer
    (switch-to-buffer js)
    (goto-char (point-max))
    (insert (format "app.markerA=0;\napp.markerB=%d;\n" (- lengthrendered 1)))
    (insert (format "app.video.setPostProc(3,3,0);\napp.video.setFps1000(25000);app.video.codec(\"Copy\",\"CQ=4\",\"0\");app.audio.reset();app.audio.codec(\"copy\",128,0,\"\");app.audio.normalizeMode=0;app.audio.normalizeValue=0;app.audio.delay=0;app.audio.mixer(\"NONE\");app.setContainer(\"AVI\");setSuccess(app.save(\"%s\"));" avi-path))
    ;; Save and close temp files
    (save-buffer js-path)
    (switch-to-buffer srt)
    (save-buffer srt-path)
    ;; Deal with Avidemux
    (start-process render-process render-process "avidemux2_cli" "--run" js-path "--video-process" "--save" avi-path "--quit")
    (kill-buffer js)
    (kill-buffer srt)
    (switch-to-buffer render-buffer)
    (goto-char old-point)))

(defun gneve-timecode-match ()
  "Match a timecode string."
  (let (timecode-string)
    (if (looking-at gneve-number-regexp)
        (goto-char (match-end 0)))
    (setq timecode-string (buffer-substring (match-beginning 0) (point)))
    (gneve-micros-to-frame (string-to-number timecode-string))))

(defun gneve-vslot-match ()
  "Match a vslot string."
  (if (looking-at gneve-vslot-regexp)
      (goto-char (match-end 0)))
  (buffer-substring (match-beginning 1) (- (point) 1)))

(defun gneve-micros-to-frame (microsec)
  "Convert microseconds to frame.  0.04 microsecond is one frame.
Argument MICROSEC microseconds."
  ;; 0.04 is one frame - divide by 0.04
  (/ microsec 0.04))

;;; List functions

(defalias 'first 'car)
(defalias 'second 'cadr)
(defalias 'rest 'cdr)

(add-to-list 'auto-mode-alist '("\\.edl\\'" . gneve-mode))

(provide 'gneve)

;;; gneve.el ends here
