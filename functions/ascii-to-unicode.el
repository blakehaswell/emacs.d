(defun me/convert-ascii-to-unicode (&optional b e)
  "Converts ascii punctuation marks (quotes, dashes, and ellipses) into their unicode equivilents."
  (interactive "r")
  (let ((output-buffer (generate-new-buffer "*ASCII to Unicode Output*")))
    (call-process-region b e "/usr/local/bin/smartypants" nil output-buffer nil "-2")
    (set-buffer output-buffer)
    (call-process-region (point-min) (point-max) "/usr/local/bin/ascii2uni" t output-buffer nil "-a" "D" "-q")
    (switch-to-buffer-other-window output-buffer)))
