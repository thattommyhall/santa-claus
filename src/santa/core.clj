(ns santa.core)

(declare join-group)

(defn start-thread
  [fn]
  (.start
   (Thread. fn)))

(defn random-delay []
  (Thread/sleep (rand-int 10)))

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

(defn thread [group id work-fn]
  (start-thread
   (fn []
     (random-delay)
     (helper group #(work-fn id))
     (recur))))

(defn deliver-toys [id]
  (println "Reindeer" id "delivering toys"))

(defn meet-in-study [id]
  (println "Elf" id "meeting in the study"))

(defn reindeer-thread [group id]
  (thread group id deliver-toys))

(defn elf-thread [group id]
  (thread group id meet-in-study))

(defn operate-gate
  [gate]
  (dosync (alter gate assoc :remaining (:max-size @gate)))
  (while (> (:remaining @gate) 0)
    ;(Thread/sleep 20)
    ))

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
(def reindeer-group (new-group 9))

(defn handle-group [group group-name]
  (let [current-group @group
        {:keys [max-size remaining in-gate out-gate]} current-group]
    (if (= 0 remaining)
      (do (dosync
           (ref-set group {:max-size max-size
                           :remaining max-size
                           :in-gate (make-gate max-size)
                           :out-gate (make-gate max-size)}))
          (println "***** With" group-name "*****")
          (operate-gate in-gate)
          (operate-gate out-gate)
          true))))

(defn santa []
  (while (handle-group elf-group "Elves")) 
  (handle-group reindeer-group "Reindeer")
  (recur))

(defn -main
  [& args]
  (dotimes [i 9]
    (do (println "Launching reindeer thread:" i)
        (reindeer-thread reindeer-group i)))
  (dotimes [i 10]
    (do (println "Launching elf thread" i)
        (elf-thread elf-group i)))
  (santa))