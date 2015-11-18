(ns hello-devcards.core
  (:require
    [goog.dom :as gdom]
    [om.next :as om :refer-macros [defui]]
    [om.dom :as dom])
  (:require-macros
    [devcards.core :as dc :refer [defcard deftest]]))

(enable-console-print!)

(defrecord Place [region city length limit])
(defrecord PlaceDisplay [region city length from to limit])

(defn make-new-limit [prev-limit limit length]
  (if limit
    (- limit length)
    (if prev-limit
      (- prev-limit length)
      nil)))

(defn add-days [date length]
  (let [new-date (js/Date. date)]
    (.setDate new-date (+ (.getDate new-date) length))
    new-date))

(defn make-place-display-date-limit [place prev-date prev-limit]
  (let [{:keys [region city length limit]} place
        new-date (add-days prev-date length)
        pretty-new-date (.toLocaleDateString new-date)
        pretty-prev-date (.toLocaleDateString prev-date)
        new-limit (make-new-limit prev-limit limit length)
        new-place (PlaceDisplay. region city length pretty-prev-date pretty-new-date new-limit)]
    [new-place
     new-date
     new-limit]))

(def app-state
  (atom
    {:app/title "Travel ideas"
     :app/editing nil
     :places/init-date (js/Date. "1/7/2016")
     :places/list [(Place. "Japan" "Tokyo" 3 90)
                   (Place. "Japan" "Nagoya" 7 nil)
                   (Place. "Japan" "Kyoto" 14 nil)
                   (Place. "Japan" "Tokyo" 7 nil)
                   (Place. "Japan" "Aomori" 1 nil)
                   (Place. "Japan" "Hakodate" 1 nil)
                   (Place. "Japan" "Sapporo" 48 nil)
                   (Place. "Hong Kong" "Hong Kong" 30 90)
                   (Place. "Taiwan" "Taipei" 7 90)
                   (Place. "USA" "Little Rock" 7 nil)]}))

(defmulti mutate om/dispatch)

(defmulti read om/dispatch)

(defmethod read :default
  [{:keys [state] :as env} key params]
  (let [st @state]
    (if-let [[_ value] (find st key)]
      {:value value}
      {:value :not-found})))

(defmethod read :places/list
  [{:keys [state] :as env} key params]
  (let [init-date (:places/init-date @state)
        places (:places/list @state)]
    {:value (->>
              (reduce
                (fn [[places prev-date prev-limit] place]
                  (let [[new-place new-date new-limit]
                        (make-place-display-date-limit place prev-date prev-limit)]
                    [(conj places new-place) new-date new-limit]))
                [[] init-date nil]
                places)
              (first))}))

(defui PlacesList
  static om/IQuery
  (query [this]
    '[:app/title :places/list])
  Object
  (render [this]
    (let [{:keys [app/title places/list]} (om/props this)]
      (dom/div nil
        (dom/title nil title)
        (dom/h1 nil title)
        (apply dom/ul nil
          (map
            (fn [place]
              (dom/li nil (str place)))
            list))))))

(def reconciler
  (om/reconciler
    {:state app-state
     :parser (om/parser {:read read :mutate mutate})}))

(om/add-root! reconciler
  PlacesList (gdom/getElement "app"))
