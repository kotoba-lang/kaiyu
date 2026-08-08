(ns kaiyu.diagnose-test
  (:require [clojure.test :refer [deftest is testing]]
            [kaiyu.core :as kaiyu]
            [kaiyu.diagnose :as dx]))

(def win (kaiyu/window "2026-08-08" {:days 7}))

(defn- report
  [{:keys [visits dwell transitions since vocabulary live-since]
    :or {since "2026-06-01"}}]
  {:site "example.test"
   :window win
   :vocabulary (or vocabulary #{"home" "video" "pricing" "signup"})
   :site-live-since live-since
   :sections {:visits (kaiyu/section win (or visits []) since)
              :dwell (kaiyu/section win (or dwell []) since)
              :transitions (kaiyu/section win (or transitions []) since)}})

(deftest a-broken-instrument-outranks-everything-else
  (testing "an unmeasured section on a site that was live suppresses the site
            findings entirely — reporting both invites acting on the ones that
            are easier to act on, which are exactly the possible artefacts"
    (let [d (dx/diagnose (assoc-in (report {:live-since "2026-01-01"
                                            :dwell [{:route "video" :bucket "lt10" :count 50}]})
                                   [:sections :transitions]
                                   (kaiyu/section win [] nil)))]
      (is (:blocked? d))
      (is (= :blocked (:severity (dx/top-finding d))))
      (is (every? #(= "kaiyu.measurement" (namespace (:id %))) (:findings d))
          "no site finding is reported while the instrument is in doubt"))))

(deftest a-measured-empty-section-is-a-question-not-a-silence
  (let [d (dx/diagnose (report {:visits [] :since "2026-06-01"}))
        ids (set (map :id (:findings d)))]
    (is (contains? ids :kaiyu.measurement/visits-empty-while-measured))
    (is (not (:blocked? d)) "measured-but-empty is answerable, so the rest still runs")))

(deftest a-site-that-was-not-live-yet-is-not-a-broken-instrument
  (testing "no rows before the site existed is the correct answer, not a fault"
    (let [d (dx/diagnose (-> (report {:live-since "2026-08-20"})
                             (assoc-in [:sections :dwell] (kaiyu/section win [] nil))
                             (assoc-in [:sections :visits] (kaiyu/section win [] nil))
                             (assoc-in [:sections :transitions] (kaiyu/section win [] nil))))]
      (is (not (:blocked? d))))))

(deftest bounce-is-reported-per-route-with-its-numbers
  (let [d (dx/diagnose (report {:dwell [{:route "pricing" :bucket "lt10" :count 45}
                                        {:route "pricing" :bucket "30_59" :count 5}
                                        {:route "video" :bucket "60_179" :count 30}]}))
        f (first (filter #(= :kaiyu.attention/bounce-pricing (:id %)) (:findings d)))]
    (is (some? f))
    (is (= :high (:severity f)))
    (is (= 50 (get-in f [:evidence :samples])))
    (is (= 45 (get-in f [:evidence :under-10s])))
    (is (re-find #"90%" (:title f)))
    (testing "a route that holds attention is not reported"
      (is (nil? (first (filter #(= :kaiyu.attention/bounce-video (:id %)) (:findings d))))))))

(deftest a-thin-sample-does-not-become-a-finding
  (testing "three visits is not a bounce rate, it is three visits"
    (let [d (dx/diagnose (report {:dwell [{:route "pricing" :bucket "lt10" :count 3}]}))]
      (is (empty? (filter #(= "kaiyu.attention" (namespace (:id %))) (:findings d)))))))

(deftest a-dead-end-is-named-and-asked-about-not-asserted
  (let [d (dx/diagnose (report {:transitions [{:from "home" :to "pricing" :count 12}
                                              {:from "home" :to "video" :count 8}
                                              {:from "video" :to "home" :count 4}]}))
        f (first (filter #(= :kaiyu.journey/dead-end-pricing (:id %)) (:findings d)))]
    (is (some? f))
    (is (= 12 (get-in f [:evidence :inbound])))
    (is (re-find #"終わってよい面か" (:question f))
        "it asks whether ending here is correct rather than declaring a defect")
    (testing "a route with an outbound edge is not a dead end"
      (is (nil? (first (filter #(= :kaiyu.journey/dead-end-video (:id %)) (:findings d))))))))

(deftest other-is-never-a-dead-end
  (testing "`other` is the overflow bucket, not a page — a dead end there says
            nothing anyone can act on"
    (let [d (dx/diagnose (report {:transitions [{:from "home" :to "other" :count 99}]}))]
      (is (empty? (filter #(= :kaiyu.journey/dead-end-other (:id %)) (:findings d)))))))

(deftest an-unreached-page-is-low-severity
  (let [d (dx/diagnose (report {:transitions [{:from "home" :to "video" :count 9}]
                                :vocabulary #{"home" "video" "signup"}}))
        f (first (filter #(= :kaiyu.reach/unreached-signup (:id %)) (:findings d)))]
    (is (some? f))
    (is (= :low (:severity f)) "a page with no traffic may be perfectly deliberate")))

(deftest one-channel-carrying-everything-is-worth-knowing
  (let [d (dx/diagnose (report {:visits [{:source "search" :count 95} {:source "direct" :count 3}]}))
        f (first (filter #(= "kaiyu.acquisition" (namespace (:id %))) (:findings d)))]
    (is (some? f))
    (is (re-find #"止まったとき" (:question f)))
    (testing "a mixed entry profile is not a finding"
      (is (empty? (filter #(= "kaiyu.acquisition" (namespace (:id %)))
                          (:findings (dx/diagnose (report {:visits [{:source "search" :count 40}
                                                                    {:source "direct" :count 35}
                                                                    {:source "social" :count 30}]})))))))))

(deftest findings-are-ranked-so-a-loop-can-take-the-first
  (let [d (dx/diagnose (report {:dwell [{:route "pricing" :bucket "lt10" :count 40}]
                                :transitions [{:from "home" :to "pricing" :count 12}]
                                :visits [{:source "search" :count 99} {:source "direct" :count 1}]
                                :vocabulary #{"home" "video" "pricing" "signup"}}))
        sev (mapv :severity (:findings d))]
    (is (= sev (vec (sort-by #(.indexOf [:blocked :high :medium :low] %) sev)))
        "severity order, so `top-finding` really is the one to work on")
    (is (= :high (:severity (dx/top-finding d))))))

(deftest nothing-to-say-is-a-valid-round
  (testing "a loop that must file an issue every round files noise on the
            rounds when the site is fine, and noise is what makes a queue unread"
    (let [d (dx/diagnose (report {:dwell [{:route "video" :bucket "60_179" :count 30}]
                                  :transitions [{:from "home" :to "video" :count 20}
                                                {:from "video" :to "home" :count 15}]
                                  :visits [{:source "search" :count 20} {:source "direct" :count 18}]
                                  :vocabulary #{"home" "video"}}))]
      (is (empty? (:findings d)))
      (is (nil? (dx/top-finding d))))))

(deftest an-issue-states-what-was-measured-and-refuses-to-guess-why
  (let [d (dx/diagnose (report {:dwell [{:route "pricing" :bucket "lt10" :count 40}]}))
        issue (dx/->issue d (dx/top-finding d))]
    (is (= :kaizen/open-issue (:kind issue)))
    (is (re-find #"^\[example\.test\]" (:title issue)))
    (is (re-find #"2026-08-02 〜 2026-08-08" (:body issue)) "the window is in the body, both ends")
    (is (re-find #"\*\*なぜ\*\*は分からない" (:body issue))
        "the issue says out loud which question the measurement cannot answer")
    (is (re-find #"kaizen:example\.test:" (:id issue)))
    (testing "the id is stable for the same finding in the same window, so a
              loop re-running does not open a second issue for one thing"
      (is (= (:id issue) (:id (dx/->issue d (dx/top-finding d))))))))
