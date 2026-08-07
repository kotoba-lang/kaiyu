(ns kaiyu.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [kaiyu.core :as kaiyu]))

;; The vocabularies the two originating sites actually shipped. Kept here as
;; fixtures so a change to this library that would alter their numbers fails
;; loudly, rather than being discovered when two products stop being comparable.
(def shinshi-routes
  #{"video" "shorts" "chat" "livechat" "doujin" "comic" "vr" "amateur"
    "subscription" "premium-feed" "support" "actresses" "categories"
    "ranking" "search" "mypage" "favorites" "history" "points" "signup"
    "login" "report" "about" "guide" "faq" "news" "contact" "terms"
    "privacy" "legal" "h" "general-video" "ebooks" "games" "shinshitv"
    "onlinesalon"})

(def babiniku-routes #{"home" "create" "chat" "watch" "gallery" "pricing"})

(deftest dwell-bucket-boundaries
  (testing "boundaries are exactly the ones club-shinshi-app and net-babiniku
            deployed on 2026-08-06 — changing them makes old rows and new rows
            incomparable, which is a data decision, not a refactor"
    (doseq [[secs expected] [[0 "lt10"] [9 "lt10"] [9.999 "lt10"]
                             [10 "10_29"] [29 "10_29"]
                             [30 "30_59"] [59 "30_59"]
                             [60 "60_179"] [179 "60_179"]
                             [180 "180_plus"] [86400 "180_plus"]]]
      (is (= expected (kaiyu/dwell-bucket secs)) (str secs "s"))))
  (testing "a missing measurement is the smallest bucket, never a crash"
    (is (= "lt10" (kaiyu/dwell-bucket nil))))
  (testing "every produced bucket is in the declared vocabulary"
    (doseq [s [0 5 15 45 100 500]]
      (is (kaiyu/valid-dwell-bucket? (kaiyu/dwell-bucket s))))))

(deftest unknown-buckets-are-refused-not-coerced
  (testing "a bogus bucket must be DROPPED by the host, not folded into a
            default — a default would pollute the very distribution the table
            exists to hold"
    (is (not (kaiyu/valid-dwell-bucket? "bogus")))
    (is (not (kaiyu/valid-dwell-bucket? "")))
    (is (not (kaiyu/valid-dwell-bucket? nil)))
    (is (not (kaiyu/valid-dwell-bucket? "LT10")))))

(deftest acquisition-buckets-carry-no-values
  (testing "campaign wins over everything: an ad posted on a social network is
            still the ad we paid for"
    (is (= "campaign" (kaiyu/acquisition-source {:campaign? true :referrer-host "bsky.app"}))))
  (is (= "direct" (kaiyu/acquisition-source {:referrer-host nil})))
  (is (= "direct" (kaiyu/acquisition-source {:referrer-host "  "})))
  (is (= "search" (kaiyu/acquisition-source {:referrer-host "www.google.co.jp"})))
  (is (= "social" (kaiyu/acquisition-source {:referrer-host "bsky.app"})))
  (is (= "social" (kaiyu/acquisition-source {:referrer-host "t.co"})))
  (is (= "referral" (kaiyu/acquisition-source {:referrer-host "example.com"})))
  (testing "an internal referrer is not an external referral"
    (is (= "direct" (kaiyu/acquisition-source {:referrer-host "www.babiniku.net"
                                               :own-hosts ["babiniku"]}))))
  (testing "output is closed"
    (doseq [h [nil "google.com" "bsky.app" "example.org" "weird"]]
      (is (contains? kaiyu/visit-sources (kaiyu/acquisition-source {:referrer-host h}))))))

(deftest normalize-route-is-a-whitelist
  (testing "every spelling of the landing page is one bucket"
    (doseq [in ["/" "home" "" "   " nil "HOME"]]
      (is (= "home" (kaiyu/normalize-route shinshi-routes in)) (pr-str in))))
  (testing "a named segment passes through, case and space folded"
    (is (= "video" (kaiyu/normalize-route shinshi-routes "video")))
    (is (= "video" (kaiyu/normalize-route shinshi-routes "  VIDEO  "))))
  (testing "identifiers, paths and typed text cannot be smuggled in"
    (doseq [in ["boa-hancock-one-piece" "/video" "/video/actress/x"
                "video?q=some+search+term" ".env" "wp-admin"
                (apply str (repeat 400 "x"))]]
      (is (= "other" (kaiyu/normalize-route shinshi-routes in)) (pr-str in))))
  (testing "the vocabulary is per-site: babiniku's routes are not shinshi's"
    (is (= "watch" (kaiyu/normalize-route babiniku-routes "watch")))
    (is (= "other" (kaiyu/normalize-route shinshi-routes "watch")))
    (is (= "other" (kaiyu/normalize-route babiniku-routes "doujin"))))
  (testing "output is closed under any input"
    (let [allowed (conj shinshi-routes "home" "other")]
      (doseq [in ["video" "nonsense" "/" nil 42 :video]]
        (is (contains? allowed (kaiyu/normalize-route shinshi-routes in)) (pr-str in))))))

(deftest transitions-drop-non-navigations
  (testing "movement inside one section is not a transition — video→video edges
            are numerous enough to bury the edges that carry signal"
    (is (nil? (kaiyu/transition shinshi-routes "video" "video")))
    (is (nil? (kaiyu/transition shinshi-routes "/video/scene/a" "/video/scene/b"))
        "both ends normalize to `other` here, and other→other is not signal")
    (is (nil? (kaiyu/transition shinshi-routes "chat" "chat"))))
  (testing "a real move is recorded with both ends bounded"
    (is (= {:from "home" :to "video"} (kaiyu/transition shinshi-routes "/" "video")))
    (is (= {:from "video" :to "other"}
           (kaiyu/transition shinshi-routes "video" "boa-hancock-scene-77"))))
  (testing "an edge is only as bounded as its weakest end"
    (is (= {:from "other" :to "ranking"}
           (kaiyu/transition shinshi-routes "/etc/passwd" "ranking")))))

(deftest window-is-inclusive-at-both-ends
  (testing "7 days ending 08-07 is 08-01..08-07 — what a human means by 今週"
    (is (= {:days 7 :from "2026-08-01" :to "2026-08-07"}
           (kaiyu/window "2026-08-07" {:days 7}))))
  (is (= {:days 1 :from "2026-08-07" :to "2026-08-07"} (kaiyu/window "2026-08-07" {:days 1})))
  (testing "an explicit end-day overrides today"
    (is (= {:days 7 :from "2026-07-25" :to "2026-07-31"}
           (kaiyu/window "2026-08-07" {:days 7 :end-day "2026-07-31"}))))
  (testing "a month boundary is not a special case"
    (is (= "2026-07-30" (:from (kaiyu/window "2026-08-05" {:days 7}))))))

(deftest window-never-inverts
  (testing "garbage in must not produce a range that runs backwards"
    (doseq [d [0 -1 -400 nil "seven"]]
      (let [{:keys [from to days]} (kaiyu/window "2026-08-07" {:days d})]
        (is (<= (compare from to) 0) (pr-str d))
        (is (pos? days) (pr-str d)))))
  (testing "clamped to the declared maximum"
    (is (= kaiyu/max-window-days (:days (kaiyu/window "2026-08-07" {:days 9999})))))
  (testing "an unparseable end-day falls back to today rather than to epoch 0"
    (is (= "2026-08-07" (:to (kaiyu/window "2026-08-07" {:end-day "not-a-day"}))))))

(deftest empty-means-three-different-things
  (let [win (kaiyu/window "2026-08-07" {:days 7})]
    (testing "never collected, or collection began after the window"
      (is (= :not-measured (kaiyu/measurement-state win nil)))
      (is (= :not-measured (kaiyu/measurement-state win "2026-09-01"))))
    (testing "collection began inside the window"
      (is (= :partial (kaiyu/measurement-state win "2026-08-06"))))
    (testing "collection covers the window"
      (is (= :measured (kaiyu/measurement-state win "2026-08-01")))
      (is (= :measured (kaiyu/measurement-state win "2026-06-11"))))
    (testing "a malformed boundary is treated as unmeasured, never as measured"
      (is (= :not-measured (kaiyu/measurement-state win "yesterday"))))))

(deftest section-pairs-rows-with-their-boundary
  (let [win (kaiyu/window "2026-08-07" {:days 7})
        s (kaiyu/section win [{:route "video" :bucket "30_59" :count 3}] "2026-08-06")]
    (is (= 1 (count (:rows s))))
    (is (= "2026-08-06" (:collected-since s)))
    (is (= :partial (:state s)))
    (testing "an empty section still says which kind of empty it is"
      (is (= :not-measured (:state (kaiyu/section win [] nil))))
      (is (= :measured (:state (kaiyu/section win [] "2026-01-01")))))))
