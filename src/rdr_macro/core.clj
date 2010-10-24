(ns rdr-macro.core)

(defn dispatch-reader-macro [ch fun]
  (let [dm (.get
            (doto
                (.getDeclaredField clojure.lang.LispReader "dispatchMacros")
              (.setAccessible true))
            nil)]
    (aset dm (int ch) fun)))


(defn dispatch-macros-char [ch]
  (let [dm (.get
            (doto
                (.getDeclaredField clojure.lang.LispReader "macros")
              (.setAccessible true))
            nil)]
    (aset dm (int ch) (clojure.lang.LispReader$DispatchReader.))))


(defn uppercase-string [rdr letter-u]
  (let [c (.read rdr)]
    (if (= c (int \"))
      (.toUpperCase (.invoke (clojure.lang.LispReader$StringReader.)
                             rdr
                             c))
      (throw (Exception. (str "Reader barfed on " (char c)))))))


(dispatch-reader-macro \U uppercase-string)

; ヒアドキュメント
(defn read-until [reader end]
  (let [end (map int end)]
    (->> (loop [res nil e end]
           (if (empty? e)
             res
             (let [c (.read reader)]
               (recur (conj res c) (if (= c (first e))
                                     (rest e)
                                     end)))))
         (drop (count end)) reverse (map char) (apply str))))

(defn here-document [reader ch]
  (let [end (read-until reader "\n")]
    (read-until reader (apply str (cons \newline end)))))

(dispatch-reader-macro \- here-document)


(defn chapter-string [rdr letter-u]
  (let [c (.read rdr)]
    (if (= c (int \"))
      (str (.invoke (clojure.lang.LispReader$StringReader.)
                    rdr
                    c))
      (throw (Exception. (str "Reader barfed on " (char c)))))))


;(defn epub-chapter [reader ch]
;  (let [end (read-until reader "\n\n")]
;    (read-until reader end)))

; !をDispathReaderに紐付ける
(dispatch-macros-char \!)
(dispatch-reader-macro \! chapter-string)

; たぶんListReaderをうまくやればいいのかな
; 以下のソースを上手く読め
; http://github.com/richhickey/clojure/blob/master/src/jvm/clojure/lang/LispReader.java

;briancarper.net (λ) - Clojure Reader Macros
; <http://briancarper.net/blog/449/>
;Clojureのリードマクロでヒアドキュメント実装してみた - 地獄の猫日記
; <http://d.hatena.ne.jp/nokturnalmortum/20100527/1274961805>
; clojure のソース
; http://github.com/richhickey/clojure/blob/master/src/jvm/clojure/lang/LispReader.java
