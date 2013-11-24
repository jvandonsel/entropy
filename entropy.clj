; entropy.clj
; Programmer: J. van Donsel
;
;Takes a file of sample text and generates new random text with similar statistical
;properties. 

(def DEPTH 3)          ; Lookahead when gathering statistics
(def PRINT_LIMIT 500)  ; Amount of new text to generate.

(def source-file "LifeOnTheMississippi-1.txt")

(println "File:" source-file " Depth:" DEPTH)

; Load our sample text
(def text (slurp source-file))

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

; partition into all possible consecuive sequences of length 'DEPTH+1'
(def parts (partition (inc DEPTH) 1 filtered-text))

;(println "Done partitioning text, length " (count filtered-text) " characters")

; sort sequences to detect duplices
(def s (sort-by #(apply str %) parts))

; split into sub-lists of identical items
(def p (partition-by identity s))

;(println "Done grouping text, " (count p) " groups")

; generate a map of unique letter sequences against their count
(def observed-map (zipmap (map first p) (map count p)))

; pretty printing
;(doseq [e (sort-by last observed-map)] (println (first e) " " (second e)))


; Pick one random letter sequence to start with. Should really do this
; based on probability. Returns a list of 'DEPTH' letters, formed
; by dropping the last letter of an observed sequence.
(def first-n-letters  (butlast (nth (keys observed-map) 
                                   (rand-int (count (keys observed-map))))))

; Function to pick a random letter
; TODO: This should be weighted 
(defn random-letter [] 
        (nth valid-chars (rand-int (count valid-chars))))

;(println "starting sequence: '" (apply str first-n-letters) "'")


; Find all the observed letter combinations that start with the last-n letters, 
; by generating all possible n+1 letter combinations starting with the last-n letters.
; Return this as pairs of strings and frequency
(defn frequency [last-n] (map #(list % (get observed-map %)) (map #(concat last-n (list %)) valid-chars)))

; Pick the next letter, given the last n
(defn next-pick [last-n]
      ; Pick a random observed string with a non-nil frequency, given the previous DEPTH letters.
      ; TODO: We should weight this with frequency
      ; Throw away the frequency part, just take the letters.
      (let [choices (remove #(nil? (last %))  (frequency last-n))
            choice (if (zero? (count choices)) nil (first (nth choices (rand-int (count choices)))))
           ]
        (if (nil?  choice)
          (do (print "*") (random-letter)) ; should never be called!
          (last choice) ; Take the last character in the new sequence
          )))

; Generate text based on statistics of the input text. Adds one character per recursion.
(defn generate [output last-n limit]
  (let [c (next-pick last-n)]
       (if (zero? limit)
           output
           (recur (concat output (list c)) (concat (drop 1 last-n) (list c)) (dec limit)))
       ))

; Run the whole thing
(print (apply str (generate [] first-n-letters PRINT_LIMIT)))


















