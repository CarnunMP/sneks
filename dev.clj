(defmulti task first)

(defmethod task :default
  [[task-name]]
  (println "Unknown task:" task-name)
  (System/exit 1))

(defmethod task "native"
  [_]
  (require '[sneks.start-dev])
  ((resolve 'sneks.start-dev/start)))

(defmethod task "repl"
  [_]
  (clojure.main/repl :init #(doto 'sneks.start-dev require in-ns)))

(task *command-line-args*)
