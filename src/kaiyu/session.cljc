(ns kaiyu.session
  "The browser-side accounting that turns page time into one dwell bucket per
  view, as a pure state machine.

  A host owns the events (visibilitychange, focus/blur, pagehide, a router
  hook) and the transport (sendBeacon). It should not also own the rules,
  because the rules are where the mistakes are, and both existing
  implementations made the same ones in the same order:

  - **counting time in a background tab.** A page left open overnight reports
    a night of attention that nobody paid.
  - **counting a tab return as the same view.** Leaving and coming back is two
    periods of attention; treating it as one loses the reading that happened
    after the return, which on a text-heavy page is most of it.
  - **double-reporting on close.** `pagehide` can fire after a route change
    already closed the view out, and fires more than once in some browsers.
  - **counting a re-entry as navigation.** Selecting the view already showing
    is not a transition.

  So this ns is a reducer: `(step state event) -> [state' emissions]`. No
  timers, no DOM, no network — a host feeds it wall-clock milliseconds and gets
  back the beacons to send. Which makes all four of the above testable without
  a browser, which is why they are pinned in the tests rather than in review
  comments."
  (:require [kaiyu.core :as kaiyu]))

(defn init
  "Initial state. `now-ms` is wall clock; `route` may be nil until the first
  navigation."
  [now-ms route]
  {:route route
   :active-seconds 0
   :since now-ms          ; when the current attention segment started, nil when paused
   :closed? (nil? route)})

(defn- accrue
  "Fold elapsed wall time into the active total and stop the clock."
  [{:keys [since active-seconds] :as state} now-ms]
  (if since
    (assoc state
           :active-seconds (+ active-seconds (max 0 (/ (- now-ms since) 1000.0)))
           :since nil)
    state))

(defn- close-view
  "Emit the dwell bucket for the view being left, once. Idempotent by design:
  `:closed?` is set before the emission is produced, so a second close (a
  pagehide after a route change) yields nothing rather than a duplicate row."
  [{:keys [route closed?] :as state} now-ms]
  (let [state (accrue state now-ms)]
    (if (or closed? (nil? route))
      [state []]
      [(assoc state :closed? true)
       [{:t :dwell :route route :bucket (kaiyu/dwell-bucket (:active-seconds state))}]])))

(defn step
  "Advance the machine. Returns `[state' emissions]`, where each emission is
  `{:t :dwell :route r :bucket b}` or `{:t :nav :from f :to t}`.

  Events:
    `{:type :navigate :to r :now n}`  — the app moved to view `r`
    `{:type :hide :now n}`            — tab hidden or window blurred
    `{:type :show :now n}`            — tab visible AND focused again
    `{:type :end :now n}`             — pagehide / unload

  `:show` starts a NEW attention segment on the same view rather than resuming
  the old one — the accrued seconds carry over, but the clock restarts, so the
  gap while the tab was elsewhere is not counted."
  [state {:keys [type to now vocabulary]}]
  (case type
    :navigate
    (let [edge (when (:route state) (kaiyu/transition (or vocabulary []) (:route state) to))]
      (if (and (:route state) (nil? edge))
        ;; Re-entering the view already showing (or another item in the same
        ;; section): not a navigation, and not a reason to close out dwell.
        [state []]
        (let [[state closes] (close-view state now)]
          [(assoc state :route to :active-seconds 0 :since now :closed? false)
           (into (vec closes) (when edge [{:t :nav :from (:from edge) :to (:to edge)}]))])))

    :hide  [(accrue state now) []]

    :show  [(assoc state :since now) []]

    :end   (close-view state now)

    [state []]))

(defn drive
  "Run a sequence of events, returning `[state' all-emissions]`. Exists so a
  host can replay a real session in a test, and so this ns's own tests read as
  scenarios instead of as a chain of `step` calls."
  [state events]
  (reduce (fn [[st acc] ev]
            (let [[st' out] (step st ev)]
              [st' (into acc out)]))
          [state []]
          events))
