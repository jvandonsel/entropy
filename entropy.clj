; entropy.clj
; Programmer: J. van Donsel
;
;Takes a file of sample text and generates new random text with similar statistical
;properties.

(use '[clojure.contrib.seq-utils])

(def depth 8)
(def source-file "LifeOnTheMississippi-1.txt")

(println "File:" source-file " Depth:" depth)

; Load our sample text
(def text (slurp source-file))

;(println "Done loading text")

; Generate a letter list
(def valid-chars (set (concat 
                    (map char (range (int \0) (int \9)))
                    '(\space \, \. \[ \] \( \) \: \; \' \-) 
                    (map char (range (int \a) (int \z)))
                    (map char (range (int \A) (int \Z)))
                    )))

; Convert newlines to spaces 
(def flattened-text (replace {\newline \space} text))

; Filter out unwanted characters from the source text
(def filtered-text (filter #(contains? valid-chars %) flattened-text))

;(println "Done filtering text:" (apply str filtered-text))

; partition into all possible consecuive sequences of length 'depth+1'
(def parts (partition (inc depth) 1 filtered-text))

;(println "Done partitioning text, length " (count filtered-text) " characters")

; sort sequences to detect duplices
(def s (sort-by #(apply str %) parts))

;(println "Done sorting text")

; split into sub-lists of identical items
(def p (partition-by identity s))

;(println "Done grouping text, " (count p) " groups")

; generate a map of unique letter sequences against their count
(def observed (zipmap (map first p) (map count p)))

;(println "Done generating observed")

; pretty printing
;(doseq [e (sort-by last observed)] (println (first e) " " (second e)))



; Pick one letter sequence to start with. Should really do this
; based on probability. Returns a list of 'depth' letters, formed
; by dropping the last letter of an observed sequence.
(def first-n-letters  (butlast (nth (keys observed) 
                                   (rand-int (count (keys observed))))))

; Function to pick a random letter
; TODO: This should be weighted 
(defn random-letter [] 
        (nth valid-chars (rand-int (count valid-chars))))

;(println "starting sequence: '" (apply str first-n-letters) "'")


; Find all the observed letter combinations that start with the last-n letters, 
; by generating all possible n+1 letter combinations starting with the last-n letters.
; Return this as pairs of strings and frequency
(defn frequency [last-n] (map #(list % (get observed %)) (map #(concat last-n (list %)) valid-chars)))

; Pick the next letter, given the last n
(defn next-pick [last-n]
      ; Pick a random observed string (starting with last-n) with a non-nil frequency.
      ; TODO: We should weight this with frequency
      ; Throw away the frequency part, just take the letters.
      (let [choices (remove #(nil? (last %))  (frequency last-n))
            choice (if (zero? (count choices)) nil (first (nth choices (rand-int (count choices)))))
           ]
        (if (nil?  choice)
          (do (print "*") (random-letter)) ; should never be called!
          (last choice) ; Take the last character in the new sequence
          )))

; Generate text based on statistics of the input text. One character per loop.
(defn generate [last-n limit]
      (let [c (next-pick last-n)]
        (doseq []
          (print c)
          (if (zero? limit)
           (println)
            (recur (concat (drop 1 last-n) (list c)) (dec limit)))
          )))

; Run the whole thing
(generate first-n-letters 500)


















