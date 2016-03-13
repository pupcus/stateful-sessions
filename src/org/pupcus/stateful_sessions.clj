;; Copyright (c) pupcus.org. All rights reserved.
;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns org.pupcus.stateful-sessions
  "Middleware for working with 'stateful' sessions."
  (:use [ring.middleware.flash :only [wrap-flash]]
        [ring.middleware.session :only [wrap-session]]))

(declare ^:dynamic *session*)
(declare ^:dynamic *flash*)

(defn- merge-session
  ""
  [request-s response-s incoming-ss outgoing-ss]
  (cond (nil? outgoing-ss)
        (cond (nil? response-s) nil
              (= response-s :empty) request-s
              :else response-s)
        
        (= outgoing-ss :empty)
        (cond (nil? response-s) (when incoming-ss {:_stateful_session incoming-ss})
              (= response-s :empty) :empty
              :else (if incoming-ss
                      (assoc response-s :_stateful_session incoming-ss)
                      response-s))
        
        :else
        (cond (nil? response-s) {:_stateful_session outgoing-ss}
              (= response-s :empty)
              (assoc request-s :_stateful_session outgoing-ss)
              :else
              (assoc response-s :_stateful_session outgoing-ss))))

(defn- response-session
  "Build the response session."
  [request response incoming-ss outgoing-ss]
  (let [outgoing-ss (cond (keyword? outgoing-ss) outgoing-ss
                          (empty? outgoing-ss) nil
                          :else outgoing-ss)
        req-s (-> request
                  :session
                  (dissoc :_stateful_session))
        res-s (if (contains? response :session)
                (:session response)
                :empty)]
    (merge-session req-s res-s incoming-ss outgoing-ss)))

(defn wrap-stateful-session*
  "Add stateful sessions to a ring handler. Does not modify the functional
   behavior of ring sessions except that returning nil will not remove
   the session if if there is stateful data. Session data stored by this
   middleware will be put into the session under the :_stateful_session key.
   Also adds map style flash support backed by Ring's flash middleware."
  [handler]
  (fn [request]
    (binding [*session* (atom (-> request :session :_stateful_session))
              *flash* (atom {:incoming (-> request :flash)})]
      (let [incoming-ss @*session*
            request (update-in request [:session] dissoc :_stateful_session)
            response (handler request)
            outgoing-flash (merge (:outgoing @*flash*)
                                  (:flash response))
            outgoing-ss @*session*
            session (response-session request
                                      response
                                      incoming-ss
                                      (if (= outgoing-ss incoming-ss)
                                        :empty
                                        outgoing-ss))]
        (when response
          (let [response (cond (= session :empty) response
                               (empty? session)
                               (assoc response :session nil)
                               :else
                               (assoc response :session session))]
            (if outgoing-flash
              (assoc response :flash outgoing-flash)
              response)))))))

(defn wrap-stateful-session
  ([handler]
     (wrap-stateful-session handler {}))
  ([handler options]
     (-> handler
         wrap-stateful-session*
         wrap-flash
         (wrap-session options))))

(defn update-session! [update-fn value]
  (swap! *session* update-fn value))

(defn session-put! [k v]
  (swap! *session* (fn [a b] (merge a {k b})) v))

(defn session-get
  ([k] (session-get k nil))
  ([k default] (if (vector? k)
                 (get-in @*session* k)
                 (get @*session* k default))))

(defn session-delete-key! [k]
  (swap! *session* (fn [a b] (dissoc a b)) k))

(defn session-pop!
  ([k] (session-pop! k nil))
  ([k default]
     (if-let [v (session-get k default)]
       (do (session-delete-key! k)
           v)
       default)))

(defn destroy-session! []
  (swap! *session* (constantly nil)))

(defn flash-put!
  "Add a value to the flash in such a way that it is available in both
   this request and the next."
  [k v]
  (swap! *flash* (fn [a b] (-> a
                                     (assoc-in [:outgoing k] b)
                                     (assoc-in [:incoming k] b))) v))

(defn flash-get
  "Get a value from the flash which may have been added during the current or
   previous request."
  [k]
  (try (-> @*flash*
           :incoming
           k)
       (catch Exception _ nil)))
