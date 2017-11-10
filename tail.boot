#!/usr/bin/env boot

(import (java.io File))
(set-env! :dependencies '[[com.nomistech/nomis-tailer "0.2.0"]
                          [org.clojure/data.json  "0.2.6"  :scope "provided"
                           :classifier "aot"]
                          [jansi-clj "0.1.0"]
                          [clj-time "0.14.0"]
                          [funcool/cuerdas "2.0.3"]
                          [com.taoensso/timbre      "4.10.0"]
                          ])

(require '[boot.core :refer :all :as core]            
         '[boot.task.built-in :refer :all]
         '[boot.util :as util]
         '[boot.cli :refer [defclifn]]
         '[clojure.java.io :as io]
         '[clojure.pprint :refer [pprint]]
         '[nomis-tailer.core :as tailer]
         '[clojure.java.io :as io]
         '[clojure.core.async :as a]
         '[clojure.data.json :as json]
         '[jansi-clj.core :as jansi]
         '[clj-time.core :as time]
         '[clj-time.format :as time-format]
         '[cuerdas.core :as str]
         '[taoensso.timbre :as timbre])

(def date-format (time-format/formatters :date))
(def date-time-format (time-format/formatters :date-time))
(def date-time-no-seconds-format (time-format/formatter "yyyy-MM-ddHH:mm"))

(defn calc-start-end [date start length]
  {:pre [(or (nil? start) (and (string? start) (re-matches #"\d\d:\d\d" start)))
         (or (nil? length) (and (string? length) (re-matches #"\d+[d|m|h|s]" length)))]}
  (try
    (let [length (when length (let [hms (condp = (last length)
                                          \d time/days
                                          \h time/hours
                                          \m time/minutes
                                          \s time/seconds)
                                    length (str/slice length 0 (dec (count length)))
                                    length (str/parse-int length)]
                                (hms length)))]
      (if start
        (let [date-time (time-format/parse date-time-format date)
              date-str (time-format/unparse date-format date-time)
              start (time-format/parse date-time-no-seconds-format (str date-str
                                                                        start))
              end (if length (time/plus start length))]
          {:start start :end end})

        {:start (if length (time/minus (time/now) length))}))
    (catch Exception e (print e))))

;; (calc-start-end "2017-10-09T06:43:35.899Z" "10:10" "2h")
;; (calc-start-end "2017-10-09T06:43:35.899Z" nil "0d")

(defn NaN?
  "Test if this number is nan"
  [x]
  ;; Nan is the only value for which equality is false
  (false? (== x x)))

(defn number-or-nil? [s]
  (or (nil? s) (number? s) (not (NaN? (str/parse-int s)))))

(defn possible-date-range? [start length]
  (not (and (number-or-nil? start)
            (number-or-nil? length))))

(defn parse-natural-number
  "Reads and returns an integer from a string, or the param itself if
  it's already a natural number. Returns nil if not a natural
  number (includes 0)"
  [s]
  (cond
    (and (string? s) (re-find #"^\d+$" s)) (read-string s)
    (and (number? s) (>= s 0))  s
    :else nil))

(defn find-min-index
  "Find minimal index in v (which should be sorted) where (compare (get v index) x) is
    true. "
  [compare x v]
  (letfn [(find-index [from subv]
            (let [middle (quot (count subv) 2)
                  middle-el (get subv middle)]
              (if (= (count subv) 1)
                from ;; (if (compare (get v from) x) from)
                (if (compare middle-el x)
                  (find-index (+ from middle) (subvec subv middle))
                  (find-index from (subvec subv 0 middle))
                  ))))]
    (find-index 0 v)))
  
(defn subvec-compare
  "Get subvec of v where all the values are between start and end as
  compared by compare.  If end is nil s"
  [v start end compare]
  {:pre [(vector? v)
         (fn? compare)
         ]}
  (let [start (if start
                (inc (or (find-min-index compare start v) -1)))
        end (if end (inc (or (find-min-index compare end v) -1)))]
    (cond
      (and start end) (subvec v start end)
      start (subvec v start)
      end (subvec v 0 end)
      :else v)))
  
  ;; (subvec-compare [0 1 2 3 4 5] 2.1 5
  ;;                 (fn [entry b]
  ;;                   (< entry b)
  ;;                   ;; (time/before?
  ;;                   ;;  (time-format/parse date-time-format (get entry "@timestamp"))
  ;;                   ;;  b)
  ;;                   )
  ;;                 )
  ;; (find-min-index (fn [a b] ( time/before?
  ;;                            (time-format/parse date-time-format a) b))
  ;;                 (time-format/parse date-time-format "2017-10-12T01:00:00.000Z")
  ;;                 ["2017-10-10T00:00:00.000Z"
  ;;                  "2017-10-11T00:00:00.000Z"
  ;;                  "2017-10-12T00:00:00.000Z"
  ;;                  "2017-10-13T00:00:00.000Z"
  ;;                  "2017-10-14T00:00:00.000Z"
  ;;                  ])
  ;; (find-min-index  #(< %1 %2) 2 [0 1 2 3 4 5])
  

(defn slurp-file
  [file]
  (str/split  (slurp file) #"\n"))

(defn parse-json [s]
  (try
    (json/read-str s)
    (catch Exception e)))

(do
 (defn parse-log-string [log-string index timestamp]
   (let [log-entry (parse-json log-string)]
     (if-not log-entry
       {:message log-string}
       (let [{:strs [namespace line message stacktrace level]
              ts "@timestamp"} log-entry]
         (assoc log-entry
                :message (str ((if (= level "error") jansi/red jansi/green) index) " "
                              (when timestamp (jansi/magenta (str ts " ")))
                              (jansi/italic (jansi/yellow (str  namespace ":" line))) " "
                              message " " stacktrace))))))
 
 (defn filter-log-entry?  [{:keys [regex level http-log]} {:keys [:message] log-level "level"}]
   (or (when (not http-log) (re-find #"web-server.handler:84" message)) 
       (and regex (not (re-find regex message)))
       (and level (not= level log-level))))

 (defn parse-log
   "Reads file and prints out all or a range. Can be filtered with a
  regex, and optional http-log boolean. If log entries are logstash
  jsons range can be by date and filtering can be on level as well. "
   [{:keys [file http-log start length regex level timestamp] :as options}] 
   (let  [log (->> file
                   slurp-file
                   (map-indexed (fn [index log-string]
                                  (parse-log-string log-string index timestamp))))
          
          log (cond->> log
                (or (not http-log) regex level)
                (remove #(filter-log-entry? options %1)))
          
          log (if (possible-date-range? start length)
                ;;range by date
                (if-let [last-timestamp (get (last log) "@timestamp")]
                  (let [{:keys [start end]} (calc-start-end last-timestamp start length)]
                    (timbre/info "start:" start " end:" end)
                    (subvec-compare (into [] log) start end
                                    (fn [log-entry t]
                                      (time/before?
                                       (time-format/parse date-time-format (get log-entry "@timestamp")) t))))
                  [])
                ;;range by number
                (let [start (when start (parse-natural-number start))
                      length (when length (parse-natural-number length))
                      log (if start (drop start log) log)]
                  (if length
                    (if start
                      (take length log)
                      (take-last length log))
                    log)))]
     (mapv :message log)))

 (defn print-log [options-map]
   (let [log (parse-log options-map)]
     (if (empty? log)
       (print "No results")
       (print (str/join "\n" log)))))

 ;; (print-log {:file "/home/michieljoris/tmp/bilby-logs/bilby.log"
 ;;             ;; :file "bilby.log"
 ;;             :http-log true
 ;;              :level "info"
 ;;             :timestamp true
 ;;             ;; :regex "starting"
 ;;             ;; :start nil
 ;;             ;;:length "100h" 
 ;;             ;; :start "09:00"
 ;;             ;; :length "4m"
 ;;             :start nil
 ;;             :length 3})
 )

(defn tail [file-name {:keys [http-log regex level timestamp] :as options} ]
  (let [tailer (tailer/make-single-file-tailer (File. file-name) 100)
        c (tailer/channel tailer)]
    ;; (a/put! c "Tailing. Control-c or enter to stop")
    (a/go-loop [line (a/<! c)]
      (let [log-entry (parse-log-string line nil timestamp)]
        (if-not (filter-log-entry? options log-entry) 
          (println (:message log-entry))
          (flush)))
      (when (not= line "stop") (recur (a/<! c))))
    c))

(defclifn -main
  [f follow bool "follow"
   s start VAL str "start (line number or time (hh:mm) such as \"11:10\")"
   n length VAL str "number of lines or length of time such as \"10h\", \"5m\" \"50s\" If start is given then last so many lines or within last so much time"
   t http-log bool "print http output lines"
   i timestamp bool "print timestamps"
   r regex VAL str "regex to filter lines"
   l level VAL str "level to filter such as info or error"]
  ;; (println "Named parameters " *opts*)
  ;; (println "List of arguments " *args*)
  ;; (println (clojure.core/first *opts*))
  (let [file (clojure.core/first *args*)
        options-map (assoc *opts* :file file)
        options-map (update options-map :regex #(cond-> %
                                                  % re-pattern))]
    (pprint options-map)
    ;; (boot (repl :port 15123))
    (print-log options-map)
    (println)
    (when follow
      (println "Tailing. Control-c or enter to stop")
      (let [c (tail file options-map)]
        (read-line)
        (a/put! c "stop")))))


