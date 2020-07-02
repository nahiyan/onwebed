(ns onwebed-cli.compiler.bone.descriptor.element.targets
  (:require [clojure.string :as string]))

;; Merge targets into one
(defn merge_
  ([parent child]
   (merge_ parent (get child :association) (get child :text-items)))
  ([accumulator remaining-associations-of-child text-items-of-child]
   (if (empty remaining-associations-of-child)
     accumulator
     (let
      [current-association-of-child (first remaining-associations-of-child)
       rest-of-associations-of-child (rest remaining-associations-of-child)
       current-association-of-child-key (first current-association-of-child)
       current-association-of-child-value (last current-association-of-child)

       exists-in-accumulator?
       (if (get accumulator current-association-of-child-key)
         true
         false)

       new-accumulator
       (if (exists-in-accumulator?)
         accumulator
         (let
          [accumulator-text-items (get accumulator :text-items)
           accumulator-associations (get accumulator :associations)
           text-items (vec (map (fn [text-item-index]
                                  (get text-items-of-child
                                       text-item-index))
                                current-association-of-child-value))
           new-text-items (vec (concat accumulator-text-items text-items))
           text-items-count (count text-items-of-child)
           association {(keyword current-association-of-child-key)
                        (range text-items-count
                               (+ text-items-count
                                  (count text-items)))}
           new-associations (merge accumulator-associations association)]
           {:text-items new-text-items
            :associations new-associations}))]
       (merge_ new-accumulator
               rest-of-associations-of-child
               text-items-of-child)))))

; Get descriptor element targets from flesh items
(defn from-flesh-items
  [flesh-items]
  (let
   [text-items (vec
                (map (fn
                       [item]
                       (get item :content))
                     flesh-items))
    association (reduce (fn
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
                                [targets (js->clj (string/split (get flesh-item :for) #"\s"))]
                                 (reduce conj {} (map (fn
                                                        [target]
                                                        {target (vector content-item-index)})
                                                      targets))))
                             flesh-items
                             (range (count text-items))))]
    {:text-items text-items
     :association association}))