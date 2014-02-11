


(defun spotifydata (term)
  (let ((spotifydata (with-current-buffer (url-retrieve-synchronously (concat "http://ws.spotify.com/search/1/track.json?q=" term))
                       (goto-char url-http-end-of-headers)
                       (json-read)))
        (tracks (cdr (assoc 'tracks spotify-data))))
    (mapcar (lambda (x) (cons (cdr (assoc 'name x))
                              (cdr (assoc 'length x)))) tracks)))

(defun packtracks (leftover tracks)
  (loop for (name . length) in (sort tracks (lambda (x y) (> (cdr x) (cdr y))))
        if (<= length leftover)
        do (setq leftover (- leftover length))
        and collect (cons name length)))


(defun getanswer (term timelimit)
  (packtracks timelimit (spotifydata term)))

(getanswer "haskell" 600)
