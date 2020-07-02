(ns onwebed-cli.compiler.bone.descriptor.element.targets
  (:require [clojure.string :as string]))

;; Merge targets into one
(defn merge_
  [new-targets remaining-targets]
  (if (empty? remaining-targets)
    new-targets
    (let
     [current-target (first remaining-targets)
      rest-of-targets (rest remaining-targets)
      current-target-key (first current-target)
      current-target-value (last current-target)

      exists-in-new-targets?
      (if (get new-targets current-target-key)
        true
        false)

      new-new-targets
      (if exists-in-new-targets?
        new-targets
        (assoc new-targets
               current-target-key
               current-target-value))]
      (merge_ new-new-targets
              rest-of-targets))))

; Get bone descriptor element targets from flesh items
(defn from-flesh-items
  ([flesh-items]
   (from-flesh-items flesh-items {}))
  ([flesh-items targets]
   (if (empty? flesh-items)
     targets
     (let
      [current-flesh-item (first flesh-items)
       rest-of-flesh-items (rest flesh-items)
       new-targets (let
                    [flesh-item-content (get current-flesh-item :content)

                     bone-descriptor-element-ids
                     (js->clj (string/split (get current-flesh-item :for)
                                            #"\s"))]
                     (reduce (fn [acc id]
                               (let
                                [existing-target (get targets (keyword id))]
                                 (assoc acc
                                        (keyword id)
                                        (str existing-target flesh-item-content))))
                             targets
                             bone-descriptor-element-ids))]
       (from-flesh-items rest-of-flesh-items new-targets)))))