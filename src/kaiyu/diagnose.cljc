(ns kaiyu.diagnose
  "What a 回遊 report SAYS is wrong — the judgment half, kept pure and separate
  from both the measuring and the acting.

  `kaiyu.core` decides what may be recorded. This decides what the recording
  means, and it is deliberately the only place that does: an orchestrator that
  both fetches and judges ends up with its rules spread through its I/O, where
  they cannot be tested and quietly become whatever the last incident made
  them.

  Every finding here is a QUESTION a human can answer, not a verdict. The data
  is counts and dates with no visitor identity (that is the whole design), so
  it can locate where attention stops and where it never arrives — it cannot
  say why. A finding that claimed to know why would be inventing the part the
  measurement deliberately does not collect.

  ## The one rule that matters

  **Absence of rows is never evidence.** `kaiyu.core/measurement-state`
  distinguishes `:not-measured` / `:partial` / `:measured`, and
  `measurement-findings` refuses to read an empty section as a fact about
  visitors. The failure this prevents is the expensive one: on 2026-08-07
  `/_metrics/audience` answered 200 with zeros for a day while the table held
  595 pageviews, because every read threw and was swallowed. A loop that judged
  that report would have filed 『誰も来ていない』 issues, and every one of them
  would have been about a broken read.

  **What turns absence back into evidence.** A `:partial` section is normally
  allowed to be empty, because collection may have started an hour ago. But
  when a sibling section began collecting on the SAME DAY OR LATER and holds
  rows, the instrument demonstrably was not silent during this section's whole
  span, and the emptiness becomes a fact about this section rather than about
  how young the collector is. Measured 2026-08-13 on kotobase.net, whose
  `transitions` had held 0 rows since 2026-08-07 while `visits` — switched on
  the same day, read in the same response — had counted 575: two of the four
  site rules had been drawing on an empty section for six days and nothing said
  so. The finding still only asks, because counts cannot separate 「誰も回遊して
  いない」 from 「辺が emit されていない」; it is `:high` rather than `:blocked`
  for the same reason.

  **What this does NOT do**, written down because the opposite claim stood here
  until 2026-08-11 and was false: the four site rules receive `:rows` and never
  see `:state`. They fire on a `:partial` section, and on a `:not-measured` one
  whose `:blocked` finding was suppressed because the site went live inside the
  window. The numbers they produce are real, but they describe a shorter period
  than the window names — measured that day on babiniku.net, whose first-ever
  candidate was a dead-end drawn from a single day of edges and labelled with a
  seven-day window.

  So every finding carries the `:sections` it drew on, `diagnose` returns
  `:coverage`, and `->issue` prints the span the evidence is true within.
  Whether a rule should fire on `:partial` at all is deliberately NOT settled
  here: gating on `:measured` would silence three of four live sites until each
  accumulates a full window, and choosing silence over a caveated number is a
  product decision, not a library one."
  (:require [clojure.string :as str]
            [kaiyu.core :as kaiyu]))

(def severities
  "Ordered. `:blocked` is not a severity of the site — it is a severity of the
  MEASUREMENT, and it outranks everything because a broken instrument makes the
  other findings meaningless rather than merely less certain."
  [:blocked :high :medium :low])

(def ^:private severity-rank (zipmap severities (range)))

(defn- finding
  "`sections` are the report sections this finding was drawn from. They are
  carried so `->issue` can state the span the evidence actually covers, which
  is not the same as the span that was asked for: a rule reading a `:partial`
  section produces real numbers about a shorter period than the window names."
  [id severity title question evidence sections]
  {:id id :severity severity :title title :question question :evidence evidence
   :sections (vec sections)})

;; ───────────────────────── rules ─────────────────────────

(defn- live-collection-witness
  "The sibling section whose rows prove the instrument was reporting DURING
  this section's collecting span, or nil.

  Only a sibling that began collecting no earlier than this one qualifies. Its
  span is then contained in this one's, so its rows cannot all predate the
  moment this section was switched on — which is exactly what a sibling that
  started earlier fails to rule out. Sorted by name so the witness a finding
  cites is the same one on every platform the report is read on."
  [sections since]
  (->> sections
       (keep (fn [[k {:keys [collected-since rows]}]]
               (when (and (seq rows)
                          (some? collected-since)
                          (>= (compare collected-since since) 0))
                 {:section k :collected-since collected-since :rows (count rows)})))
       (sort-by (comp name :section))
       first))

(defn measurement-findings
  "Rules about the instrument, checked before any rule about the site.

  A section that is `:not-measured` inside a window the site was live for is
  either a beacon that stopped or a read that throws — both are urgent, and
  neither is a fact about visitors."
  [{:keys [window sections site-live-since]}]
  (keep (fn [[section {:keys [state collected-since rows]}]]
          (let [witness (when (and (= :partial state) (empty? rows) (some? collected-since))
                          (live-collection-witness sections collected-since))]
          (cond
            (and (= :not-measured state)
                 (or (nil? site-live-since)
                     (<= (compare site-live-since (:from window)) 0)))
            (finding (keyword "kaiyu.measurement" (str (name section) "-not-measured"))
                     :blocked
                     (str (name section) " が測れていない")
                     (str "この window で " (name section)
                          " は 1 行も無く、収集開始日も記録されていない。"
                          "beacon が動いていないのか、読み取りが失敗しているのか、"
                          "本当に誰も来ていないのか——先にこれを決めないと、他の所見は読めない。")
                     {:section section :state state :collected-since collected-since
                      :window window}
                     [section])

            (and (= :measured state) (empty? rows))
            (finding (keyword "kaiyu.measurement" (str (name section) "-empty-while-measured"))
                     :high
                     (str (name section) " は測れているのに 0 行")
                     (str "収集は " collected-since " から続いているのに、この window の行が 0。"
                          "計測は生きていて到達が無い、という読みでよいか。")
                     {:section section :collected-since collected-since :window window}
                     [section])

            witness
            (finding (keyword "kaiyu.measurement" (str (name section) "-empty-while-collecting"))
                     :high
                     (str (name section) " は収集が動いているのに 0 行")
                     (str "収集は " collected-since " から動いているのに、この window の "
                          (name section) " は 0 行。同じ日以降に始まった "
                          (name (:section witness)) " は " (:rows witness)
                          " 行を記録しているので、計器そのものは黙っていない。"
                          "この面が本当に 0 なのか、この section だけが記録されていないのか。")
                     {:section section :state state :collected-since collected-since
                      :witness (:section witness)
                      :witness-collected-since (:collected-since witness)
                      :witness-rows (:rows witness)
                      :window window}
                     [section])

            :else nil)))
        sections))

(defn dead-end-findings
  "Routes people reach and never leave.

  A route with inbound edges and no outbound ones is where the journey stops.
  That is not automatically bad — a checkout confirmation SHOULD end it — so
  the finding names the route and asks, rather than asserting a defect."
  [{:keys [transitions min-visits]}]
  (let [min-visits (or min-visits 5)
        inbound (reduce (fn [m {:keys [to count]}] (update m to (fnil + 0) count)) {} transitions)
        outbound (reduce (fn [m {:keys [from count]}] (update m from (fnil + 0) count)) {} transitions)]
    (->> inbound
         (filter (fn [[route n]]
                   (and (>= n min-visits)
                        (zero? (get outbound route 0))
                        (not= route kaiyu/other-route))))
         (sort-by (comp - val))
         (map (fn [[route n]]
                (finding (keyword "kaiyu.journey" (str "dead-end-" route))
                         :medium
                         (str "「" route "」から先に進んだ人がいない")
                         (str n " 件の遷移がこの面に入り、出ていく辺が 1 件も無い。"
                              "ここで終わってよい面か（完了・退出が正しい面か）、"
                              "それとも次に行く先が見えていないのか。")
                         {:route route :inbound n :outbound 0}
                         [:transitions])))
         vec)))

(defn attention-findings
  "Routes people reach and leave immediately.

  Reads the dwell distribution per route: if the shortest bucket dominates a
  route that is not a redirect-ish surface, the page is either not answering
  the question that brought them or not showing that it does."
  [{:keys [dwell min-samples short-share]}]
  (let [min-samples (or min-samples 10)
        threshold (or short-share 0.7)
        by-route (group-by :route dwell)]
    (->> by-route
         (keep (fn [[route rows]]
                 (let [total (reduce + 0 (map :count rows))
                       shortest (reduce + 0 (map :count (filter #(= "lt10" (:bucket %)) rows)))]
                   (when (and (>= total min-samples)
                              (>= (/ (double shortest) total) threshold))
                     (finding (keyword "kaiyu.attention" (str "bounce-" route))
                              :high
                              (str "「" route "」は " (Math/round (* 100.0 (/ (double shortest) total)))
                                   "% が 10 秒未満で離れている")
                              (str total " 件中 " shortest " 件が最短バケット。"
                                   "この面は来た人の問いに答えているか、"
                                   "答えていることが見えているか。")
                              {:route route :samples total :under-10s shortest}
                              [:dwell])))))
         (sort-by (comp - :samples :evidence))
         vec)))

(defn reach-findings
  "Routes that exist and nobody arrives at.

  Named from the site's own vocabulary, so the absence is about a page that was
  built — not about a name nobody chose. Low severity on purpose: a page with
  no traffic may be perfectly deliberate."
  [{:keys [vocabulary transitions dwell]}]
  (let [seen (into #{} (concat (map :to transitions) (map :from transitions) (map :route dwell)))]
    (->> (sort (remove seen vocabulary))
         (map (fn [route]
                (finding (keyword "kaiyu.reach" (str "unreached-" route))
                         :low
                         (str "「" route "」に誰も来ていない")
                         (str "この window で " route " への遷移も滞在も 1 件も無い。"
                              "導線が無いのか、要らない面なのか。")
                         {:route route}
                         [:transitions :dwell])))
         vec)))

(defn share-pct
  "`count` of `total` as a whole percent, floored.

  Rounding reads 4756 of 4775 as 100%, and 100% is a claim these numbers do not
  make: the 19 visits it rounds away are the whole of what the question asks
  (「この入口が止まったときに残るものがあるか」). Flooring can only understate a
  share, so a title reaches 100% only when the tail really is empty and never
  answers the question ahead of the reader."
  [count total]
  (int (Math/floor (* 100.0 (/ (double count) total)))))

(defn entry-concentration-findings
  "One arrival bucket carrying nearly everything.

  Not a defect — most sites have one dominant channel — but it is the single
  fact most worth knowing before spending on any other channel, and it changes
  what every other finding is worth."
  [{:keys [visits min-total dominant-share]}]
  (let [total (reduce + 0 (map :count visits))
        threshold (or dominant-share 0.9)]
    (when (>= total (or min-total 20))
      (when-let [{:keys [source count]} (->> visits (sort-by (comp - :count)) first)]
        (when (>= (/ (double count) total) threshold)
          [(finding (keyword "kaiyu.acquisition" (str "single-channel-" source))
                    :medium
                    (str "到達の " (share-pct count total) "% が「" source "」")
                    (str total " 件中 " count " 件が 1 つの入口。"
                         "この入口が止まったときに残るものがあるか。")
                    {:source source :visits count :total total}
                    [:visits])])))))

;; ───────────────────────── entry point ─────────────────────────

(defn coverage
  "section → `{:state :collected-since}`, the boundary each section's rows are
  true within.

  Kept beside the findings rather than folded into them because a finding is a
  question about the site and this is a fact about the instrument; a reader who
  cannot see both is being asked to trust a number without its span."
  [sections]
  (reduce-kv (fn [m k {:keys [state collected-since]}]
               (assoc m k {:state state :collected-since collected-since}))
             {} (into {} sections)))

(defn diagnose
  "A kaiyu report → ranked findings.

  `report` is what a site's read face returns, normalized to:

    {:site \"shinshi.club\"
     :window {...}                       ; kaiyu.core/window
     :vocabulary #{...}                  ; the site's route set
     :site-live-since \"YYYY-MM-DD\"     ; optional
     :sections {:visits {...} :dwell {...} :transitions {...}}}

  where each section is a `kaiyu.core/section` map.

  **Measurement findings short-circuit everything.** If the instrument is in
  doubt, the site findings are not reported at all — not sorted below, not
  included. Reporting both would invite acting on the ones that are easier to
  act on, which are exactly the ones that might be artefacts."
  [{:keys [sections vocabulary] :as report}]
  (let [measurement (vec (measurement-findings report))
        blocked (filter #(= :blocked (:severity %)) measurement)]
    (if (seq blocked)
      {:site (:site report)
       :window (:window report)
       :coverage (coverage sections)
       :findings (vec (sort-by (comp severity-rank :severity) measurement))
       :blocked? true}
      (let [rows (fn [k] (get-in sections [k :rows] []))
            findings (concat measurement
                             (attention-findings {:dwell (rows :dwell)})
                             (dead-end-findings {:transitions (rows :transitions)})
                             (entry-concentration-findings {:visits (rows :visits)})
                             (reach-findings {:vocabulary (or vocabulary #{})
                                              :transitions (rows :transitions)
                                              :dwell (rows :dwell)}))]
        {:site (:site report)
         :window (:window report)
         :coverage (coverage sections)
         :findings (vec (sort-by (juxt (comp severity-rank :severity) :id) findings))
         :blocked? false}))))

(defn top-finding
  "The one thing to work on. Returns nil when there is nothing to say — which
  a loop must treat as a valid outcome and not as a reason to lower the bar.
  A loop that must produce an issue every round will produce noise on the
  rounds when the site is fine, and noise is what makes a queue unread."
  [diagnosis]
  (first (:findings diagnosis)))

(def ^:private state-rank {:not-measured 0 :partial 1 :measured 2})

(defn- coverage-line
  "The 測ったこと line stating the span the evidence is actually true within.

  Reports the WORST state among the sections the finding drew on: a finding
  built from a fully measured section and a partial one is only as good as the
  partial one. nil when there is nothing to say — a caller that built a
  diagnosis by hand gets the old body rather than a fabricated boundary."
  [window coverage sections]
  (when-let [cs (seq (keep coverage sections))]
    (let [worst (apply min-key (comp state-rank :state) cs)
          since (:collected-since worst)]
      (case (:state worst)
        :measured
        (str "- 根拠の範囲: この window 全体（" since " から測れている）")
        :partial
        (str "- 根拠の範囲: **" since " 以降のみ**。"
             "window は " (:from window) " からだが、それ以前の行は無い。"
             "この数字を window 全体の話として読まないこと")
        :not-measured
        (str "- 根拠の範囲: **不明**（収集開始日が記録されていない）。"
             "この数字が何日分なのかは、この報告からは言えない")
        nil))))

(defn ->issue
  "A finding → the issue body a human reads in the queue.

  Deliberately not a fix. The data cannot say why, so an issue that proposed a
  remedy would be dressing a guess as an inference. It states what was
  measured, over what window, and the question to answer."
  [{:keys [site window coverage]} {:keys [id severity title question evidence sections]}]
  {:kind :kaizen/open-issue
   :id (str "kaizen:" site ":" (name id) ":" (:from window) "_" (:to window))
   :severity severity
   :title (str "[" site "] " title)
   :body (str/join
          "\n"
          (remove
           nil?
           [(str "## 測ったこと")
           (str "- サイト: " site)
           (str "- 期間: " (:from window) " 〜 " (:to window) "（両端含む）")
           (str "- 根拠: " (pr-str evidence))
           (coverage-line window coverage sections)
           ""
           "## 答えるべき問い"
           question
           ""
           "## この issue が言っていないこと"
           (str "回遊の計測は counts と dates だけで、訪問者を識別しない。"
                "どこで注意が止まり、どこに届いていないかは分かるが、"
                "**なぜ**は分からない。原因の推測はここには書かない。")]))})
