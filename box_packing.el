;; Entry to West London hack night challenge on 11/02/2014
;;
;; The challenge was to download a list of songs from spotify that
;; match a given string, and then try to fill a given period of time
;; with some of these songs as tightly as possible.

;; First attempt was using Haskell, but we had problems getting the json data
;; in a useable form, so we did a rush job with elisp at the last minute.
;; Our algorithm is very simple:
;; 1) Download the songs
;; 2) Create many different random permutations of the songs
;; 3) For each permutation take songs from the front of the list
;;    while the time period they occupy is greater than the allowed time
;; 4) Find the one which has the longest total time (and hence fits the
;;    time period the tightest).

(defun spotifydata (term)
  "Download list of songs related to the string TERM."
  (let* ((spotifydata (with-current-buffer (url-retrieve-synchronously (concat "http://ws.spotify.com/search/1/track.json?q=" term))
                        (goto-char url-http-end-of-headers)
                        (json-read)))
         (tracks (cdr (assoc 'tracks spotifydata))))
    (mapcar (lambda (x) (cons (cdr (assoc 'name x))
                              (cdr (assoc 'length x)))) tracks)))

(defun packtracks (leftover tracks)
  "Return first few elements of TRACKS that fit into time allowance LEFTOVER."
  (loop for (name . length) in tracks
        if (<= length leftover)
        do (setq leftover (- leftover length))
        and collect (cons name length)))

(defun shuffle-list (list)
  "Randomly permute the elements of LIST.
All permutations equally likely."
  (let ((i 0) j temp
        (len (length list)))
    (while (< i len)
      (setq j (+ i (random (- len i))))
      (setq temp (nth i list))
      (setcar (nthcdr i list) (nth j list))
      (setcar (nthcdr j list) temp)
      (setq i (1+ i))))
  list)

;(setq origdata (spotifydata "badger"))

(defun getanswer (term timelimit numtries)
  "Find list of songs related to string TERM whose total length is <= TIMELIMIT seconds.
NUMTRIES is the number of random permutations to try."
  (let* ((origdata (spotifydata term))
         (sdata origdata)
         (maxval 0)
         (finaltracks nil)
         (tracks
          (loop for i from 1 to numtries
                for s = (shuffle-list sdata)
                for res = (packtracks timelimit s)
                for names = (mapcar 'car res)
                for times = (mapcar 'cdr res)
                for sumtime = (apply '+ times)
                do (setq sdata origdata)
                if (> sumtime maxval)
                do (setq maxval sumtime
                         finaltracks names))))
    (cons maxval finaltracks)))

;(getanswer "badger" 1000 100000)
;(999.998 "Badger" "Polyester" "The Badger - Live Version" "Badger" "Goodbye Josh" "Badger")



  


