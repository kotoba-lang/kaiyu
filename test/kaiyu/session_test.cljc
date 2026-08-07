(ns kaiyu.session-test
  "Each test here is one of the four mistakes both original implementations
  made, replayed as a scenario. They are scenarios rather than unit assertions
  because every one of them is invisible in a unit test of the pieces: the
  pieces are all correct, and the session is what goes wrong."
  (:require [clojure.test :refer [deftest is testing]]
            [kaiyu.session :as session]))

(def routes #{"home" "video" "chat" "ranking"})

(defn- run [events]
  (second (session/drive (session/init 0 nil) events)))

(def ^:private sec 1000)

(deftest a-background-tab-does-not-accrue-attention
  (testing "a page left open overnight must not report a night of attention"
    (let [out (run [{:type :navigate :to "video" :now 0 :vocabulary routes}
                    {:type :hide :now (* 5 sec)}
                    ;; eight hours in a background tab
                    {:type :show :now (* 28805 sec)}
                    {:type :end :now (* 28810 sec)}])]
      (is (= [{:t :dwell :route "video" :bucket "10_29"}] out)
          "5s before hiding + 5s after returning = 10s of attention, not 8 hours"))))

(deftest returning-to-a-tab-starts-a-new-attention-segment
  (testing "the reading that happens after coming back is most of the reading"
    (let [out (run [{:type :navigate :to "video" :now 0 :vocabulary routes}
                    {:type :hide :now (* 2 sec)}
                    {:type :show :now (* 600 sec)}
                    {:type :end :now (* 640 sec)}])]
      (is (= [{:t :dwell :route "video" :bucket "30_59"}] out)
          "2s + 40s = 42s; counting the 10 idle minutes would say 180_plus"))))

(deftest closing-twice-reports-once
  (testing "pagehide can fire after a route change already closed the view out,
            and fires more than once in some browsers"
    (let [[state out1] (session/drive (session/init 0 nil)
                                      [{:type :navigate :to "video" :now 0 :vocabulary routes}
                                       {:type :end :now (* 15 sec)}])
          [_ out2] (session/drive state [{:type :end :now (* 20 sec)}])]
      (is (= 1 (count out1)))
      (is (= [] out2) "the second close emits nothing"))))

(deftest re-entering-the-same-view-is-not-navigation
  (let [out (run [{:type :navigate :to "video" :now 0 :vocabulary routes}
                  {:type :navigate :to "video" :now (* 30 sec) :vocabulary routes}
                  {:type :end :now (* 60 sec)}])]
    (is (= [{:t :dwell :route "video" :bucket "60_179"}] out)
        "one view, one dwell of 60s, and no self-edge")))

(deftest a-real-navigation-closes-dwell-and-records-the-edge
  (let [out (run [{:type :navigate :to "home" :now 0 :vocabulary routes}
                  {:type :navigate :to "video" :now (* 12 sec) :vocabulary routes}
                  {:type :navigate :to "chat" :now (* 200 sec) :vocabulary routes}
                  {:type :end :now (* 205 sec)}])]
    (is (= [{:t :dwell :route "home" :bucket "10_29"}
            {:t :nav :from "home" :to "video"}
            {:t :dwell :route "video" :bucket "180_plus"}
            {:t :nav :from "video" :to "chat"}
            {:t :dwell :route "chat" :bucket "lt10"}]
           out)
        "dwell is closed BEFORE the edge is emitted, so a reader sees the visit
         in the order it happened")))

(deftest the-first-view-has-no-inbound-edge
  (testing "an arrival is not a transition from nowhere"
    (let [out (run [{:type :navigate :to "home" :now 0 :vocabulary routes}
                    {:type :end :now (* 3 sec)}])]
      (is (= [{:t :dwell :route "home" :bucket "lt10"}] out)))))

(deftest an-unnamed-destination-still-produces-a-bounded-edge
  (let [out (run [{:type :navigate :to "video" :now 0 :vocabulary routes}
                  {:type :navigate :to "boa-hancock-scene-77" :now (* 5 sec) :vocabulary routes}
                  {:type :end :now (* 8 sec)}])]
    (is (= [{:t :dwell :route "video" :bucket "lt10"}
            {:t :nav :from "video" :to "other"}
            {:t :dwell :route "boa-hancock-scene-77" :bucket "lt10"}]
           out)
        "the EDGE is normalized here; normalizing the dwell route is the host's
         write path, which re-checks everything anyway")))

(deftest a-session-that-never-navigates-still-reports-its-dwell
  (testing "a visitor who lands and reads without clicking is the common case
            on a shared link, and reporting nothing for them would read as an
            instant bounce"
    (let [out (run [{:type :navigate :to "home" :now 0 :vocabulary routes}
                    {:type :end :now (* 95 sec)}])]
      (is (= [{:t :dwell :route "home" :bucket "60_179"}] out)))))
