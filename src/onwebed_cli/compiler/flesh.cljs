(ns onwebed-cli.compiler.flesh
  (:require [clojure.string :refer [split]]))

; Generate map of flesh targets and their respective contents
(defn to-descriptor-element-targets
  [flesh-items]
  (let
   [content-items (vec
                   (map (fn
                          [item]
                          (get item :content))
                        flesh-items))
    targets (reduce (fn
                      [acc, item]
                      (merge acc
                             (reduce conj
                                     {}
                                     (map (fn
                                            [keyValueList]
                                            (let
                                             [key (first keyValueList)
                                              value (last keyValueList)
                                              existing-key (get acc key)]
                                              (if (not= existing-key nil)
                                                ;; If key already exists
                                                {key (conj existing-key (first value))}
                                                {key value})))
                                          item))))
                    {}
                    (map (fn
                           [flesh-item content-item-index]
                           (let
                            [targets (js->clj (split (get flesh-item :for) #"\s"))]
                             (reduce conj {} (map (fn
                                                    [target]
                                                    {target (vector content-item-index)})
                                                  targets))))
                         flesh-items
                         (range (count content-items))))]
    {:content-items content-items
     :targets targets}))