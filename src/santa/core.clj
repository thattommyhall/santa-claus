(ns santa.core)

(declare join-group)

(defn start-thread
  [fn]
  (.start
   (Thread. fn)))

(defn random-delay []
  (Thread/sleep (rand-int 100)))

(defn make-gate
  [max-size]
  (ref {:max-size max-size
        :remaining 0}
       :validator #(>= (:remaining %) 0)))

(defn pass-gate
  [gate]
  (try 
    (dosync (alter gate #(update-in % [:remaining] dec)))
    (catch IllegalStateException e
      (do (Thread/sleep 200)
          (pass-gate gate)))))

(defn helper
  [group task]
  (let [[in-gate out-gate] (join-group group)]
    (do (pass-gate in-gate)
        (task)
        (pass-gate out-gate)
        )))

(defn deliver-toys [id]
  (println "Reindeer" id "delivering toys"))

(defn meet-in-study [id]
  (println "Elf" id "meeting in the study"))

(defn elf
  [group id]
  (helper group #(meet-in-study id)))

(defn elf-thread [group id]
  (start-thread
   #(do (random-delay)
        (elf group id)
        (recur)
        )))

(defn reindeer
  [group id]
  (helper group #(deliver-toys id)))

(defn reindeer-thread [group id]
  (start-thread
   #(do (random-delay)
        (reindeer group id)
        (recur))))

(defn operate-gate
  [gate]
  (dosync (alter gate assoc :remaining (:max-size @gate)))
  (while (> (:remaining @gate) 0)
    (Thread/sleep 20))
  )

(defn new-group
  [max-size]
  (ref {:max-size max-size
        :remaining max-size
        :in-gate (make-gate max-size)
        :out-gate (make-gate max-size)
        }
       :validator #(>= (:remaining %) 0)))

(defn join-group                  
  [group]
  (dosync (let [{:keys [remaining max-size]} @group]
            (if (= 0 remaining)
              (do (Thread/sleep 20)
                  (join-group group)))
            
            (alter group #(update-in % [:remaining] dec))
            [(:in-gate @group) (:out-gate @group)])))

(def elf-group (new-group 3))
(def rein-group (new-group 9))

(defn santa []
  (let [e-group @elf-group
        r-group @rein-group]
    (if (= 0 (:remaining e-group))
      (let [max-size (:max-size e-group)]
        (dosync
         (ref-set elf-group {:max-size max-size
                             :remaining max-size
                             :in-gate (make-gate max-size)
                             :out-gate (make-gate max-size)}))
        
        (let [in (:in-gate e-group)
              out (:out-gate e-group)]
          (do (println "***** With Elves *****")
              (operate-gate in)
              (operate-gate out))))
                                        ;)
      (if (= 0 (:remaining  r-group))
        (let [max-size (:max-size r-group)]
          (dosync
           (ref-set rein-group {:max-size max-size
                               :remaining max-size
                               :in-gate (make-gate max-size)
                               :out-gate (make-gate max-size)}))
          
          (let [in (:in-gate  r-group)
                out (:out-gate  r-group)]
            (do (println "***** with reindeer *****")
                (operate-gate in)
                (operate-gate out)))))))
  (recur))

(defn -main
  [& args]
  (dotimes [i 9] (do (println "Launching reindeer thread:" i)
                     (reindeer-thread rein-group i)))
  (dotimes [i 20] (do (println "Launching elf thread" i)
                      (elf-thread elf-group i)))
  (santa)
  )