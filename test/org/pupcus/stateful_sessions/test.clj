;; Copyright (c) pupcus.org. All rights reserved.
;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns org.pupcus.stateful-sessions.test
  (:require ring.middleware.flash)
  (:use [clojure.test :only [deftest testing is]]
        [org.pupcus.stateful-sessions]
        [ring.middleware.session.store :only [read-session]]
        [ring.middleware.session.memory :only [memory-store]]))

(def session-key :_stateful_session)

(deftest response-session-test
  (let [tst #'org.pupcus.stateful-sessions/response-session]
    (testing "stateful-session is nil"
       (testing "response session is nil"
          (let [tst (fn [request-s incoming-ss]
                      (tst request-s
                           {:session nil}
                           incoming-ss
                           nil))]
            (is (= (tst {:session {:a "a"}} {:x "x"}) nil))
            (is (= (tst {:session {:a "a"}} nil) nil))
            (is (= (tst {} {:x "x"}) nil))
            (is (= (tst {} nil) nil))))
       
       (testing "response session is empty"
          (let [tst (fn [request-s incoming-ss]
                      (tst request-s
                           {}
                           incoming-ss
                           nil))]
            (is (= (tst {:session {:a "a"}} {:x "x"}) {:a "a"}))
            (is (= (tst {:session {:a "a"}} nil) {:a "a"}))
            (is (= (tst {} {:x "x"}) nil))
            (is (= (tst {} nil) nil))))

       (testing "response session exists"
          (let [tst (fn [request-s incoming-ss]
                      (tst request-s
                           {:session {:b "b"}}
                           incoming-ss
                           nil))]
            (is (= (tst {:session {:a "a"}} {:x "x"}) {:b "b"}))
            (is (= (tst {:session {:a "a"}} nil) {:b "b"}))
            (is (= (tst {} {:x "x"}) {:b "b"}))
            (is (= (tst {} nil) {:b "b"})))))

    (testing "stateful-session is empty"
       (testing "response session is null"
          (let [tst (fn [request-s incoming-ss]
                      (tst request-s
                           {:session nil}
                           incoming-ss
                           :empty))]
            (is (= (tst {:session {:a "a"}} {:x "x"}) {session-key {:x "x"}}))
            (is (= (tst {:session {:a "a"}} nil) nil))
            (is (= (tst {} {:x "x"}) {session-key {:x "x"}}))
            (is (= (tst {} nil) nil))))
       (testing "response session is empty"
          (let [tst (fn [request-s incoming-ss]
                      (tst request-s
                           {}
                           incoming-ss
                           :empty))]
            (is (= (tst {:session {:a "a"}} {:x "x"}) :empty))
            (is (= (tst {:session {:a "a"}} nil) :empty))
            (is (= (tst {} {:x "x"}) :empty))
            (is (= (tst {} nil) :empty))))
       (testing "response session exists"
          (let [tst (fn [request-s incoming-ss]
                      (tst request-s
                           {:session {:b "b"}}
                           incoming-ss
                           :empty))]
            (is (= (tst {:session {:a "a"}} {:x "x"})
                   {:b "b" session-key {:x "x"}}))
            (is (= (tst {:session {:a "a"}} nil) {:b "b"}))
            (is (= (tst {} {:x "x"})
                   {:b "b" session-key {:x "x"}}))
            (is (= (tst {} nil) {:b "b"})))))
    
    (testing "stateful-session exists"
       (testing "response session is nil"
          (let [tst (fn [request-s incoming-ss]
                      (tst request-s
                           {:session nil}
                           incoming-ss
                           {:y "y"}))]
            (is (= (tst {:session {:a "a"}} {:x "x"})
                   {session-key {:y "y"}}))
            (is (= (tst {:session {:a "a"}} nil)
                   {session-key {:y "y"}}))
            (is (= (tst {} {:x "x"})
                   {session-key {:y "y"}}))
            (is (= (tst {} nil)
                   {session-key {:y "y"}}))))
       (testing "response session is empty"
          (let [tst (fn [request-s incoming-ss]
                      (tst request-s
                           {}
                           incoming-ss
                           {:y "y"}))]
            (is (= (tst {:session {:a "a"}} {:x "x"})
                   {:a "a" session-key {:y "y"}}))
            (is (= (tst {:session {:a "a"}} nil)
                   {:a "a" session-key {:y "y"}}))
            (is (= (tst {} {:x "x"})
                   {session-key {:y "y"}}))
            (is (= (tst {} nil)
                   {session-key {:y "y"}}))))
       (testing "response session is exists"
          (let [tst (fn [request-s incoming-ss]
                      (tst request-s
                           {:session {:b "b"}}
                           incoming-ss
                           {:y "y"}))]
            (is (= (tst {:session {:a "a"}} {:x "x"})
                   {:b "b" session-key {:y "y"}}))
            (is (= (tst {:session {:a "a"}} nil)
                   {:b "b" session-key {:y "y"}}))
            (is (= (tst {} {:x "x"})
                   {:b "b" session-key {:y "y"}}))
            (is (= (tst {} nil)
                   {:b "b" session-key {:y "y"}})))))))

(deftest wrap-stateful-session*-test
  (testing "stateful session"
     (testing "input empty, session in response"
        (is (= ((wrap-stateful-session* (fn [r] {:session {:a "a"}})) {})
               {:session {:a "a"}})))
     (testing "input empty, use session-put!"
        (is (= ((wrap-stateful-session* (fn [r] (do (session-put! :a "a")
                                                   {}))) {})
               {:session {session-key {:a "a"}}})))
     (testing "input contains values, use session-put! and return empty session"
        (is (= ((wrap-stateful-session*
                 (fn [r] (do (session-put! :a "a")
                             {})))
                {:session {:b "b"}})
               {:session {session-key {:a "a"} :b "b"}})))
     (testing "input contains values, use session-put! and return empty session"
        (is (= ((wrap-stateful-session*
                 (fn [r] (do (session-put! :a "a")
                             {:session {}})))
                {:session {:b "b"}})
               {:session {session-key {:a "a"}}})))
     (testing "input contains values, use session-put! and return empty session"
        (is (= ((wrap-stateful-session*
                 (fn [r] (do (session-put! :a "a")
                             {:session nil})))
                {:session {:b "b"}})
               {:session {session-key {:a "a"}}})))
     (testing "input contains values, use sesion-put! and return nil session"
        (is (= ((wrap-stateful-session*
                 (fn [r] (do (session-put! :a "a")
                             {:session nil})))
                {:session {:b "b"}})
               {:session {session-key {:a "a"}}})))
     (testing "input contains values, session in response replaces values"
        (is (= ((wrap-stateful-session*
                 (fn [r] {:session {:a "a"}}))
                {:session {:b "b"}})
               {:session {:a "a"}})))
     (testing "input empty, use session-put! and return session"
        (is (= ((wrap-stateful-session*
                 (fn [r] (do (session-put! :a "a")
                             {:session {:b "b"}})))
                {})
               {:session {session-key {:a "a"} :b "b"}})))
     (testing "input contains values, use session-put! and return session"
        (is (= ((wrap-stateful-session*
                 (fn [r] (do (session-put! :a "a")
                             {:session {:b "b"}})))
                {:session {:c "c"}})
               {:session {session-key {:a "a"} :b "b"}})))
     (testing "if session is not changed then do not include :session in response"
        (is (= ((wrap-stateful-session* (fn [r] {}))
                {:session {:a "a"}})
               {})))
     (testing "if session is not changed then do not include :session in response"
        (is (= ((wrap-stateful-session* (fn [r] {}))
                {:session {:a "a" session-key {:b "b"}}})
               {})))
     (testing "input contains values, return session nil removes values"
        (is (= ((wrap-stateful-session* (fn [r] {:session nil}))
                {:session {:a "a"}})
               {:session nil})))
     (testing "input contains values, return session nil removes only func values"
        (is (= ((wrap-stateful-session* (fn [r] {:session nil}))
                {:session {:a "a" session-key {:a "a"}}})
               {:session {session-key {:a "a"}}})))
     (testing "session-delete-key! does not remove values from functional session"
        (is (= ((wrap-stateful-session*
                 (fn [r] (do (session-delete-key! :a)
                             {})))
                {:session {:a "a"}})
               {})))
     (testing "session-delete-key! does remove values from stateful-session"
        (is (= ((wrap-stateful-session*
                 (fn [r] (do (session-delete-key! :a)
                             {})))
                {:session {session-key {:a "a"} :a "a"}})
               {:session {:a "a"}})))
     (testing "session-delete-key! causes session to be deleted when it is empty"
        (is (= ((wrap-stateful-session*
                 (fn [r] (do (session-delete-key! :a)
                             {})))
                {:session {session-key {:a "a"}}})
               {:session nil})))
     (testing "session-delete-key! causes session to be deleted when it is empty, when combined with wrap-flash"
        (is (= ((wrap-stateful-session*
                 (ring.middleware.flash/wrap-flash
                  (fn [r] (do (session-delete-key! :a)
                              {}))))
                {:session {session-key {:a "a"}}})
               {:session nil})))
     (testing "destroy-session! works"
        (is (= ((wrap-stateful-session*
                 (fn [r] (do (destroy-session!)
                             {})))
                {:session {session-key {:a "a" :b "b"}}})
               {:session nil})))
     (testing "destroy-session! only deletes things in the stateful session"
        (is (= ((wrap-stateful-session*
                 (fn [r] (do (destroy-session!)
                             {})))
                {:session {session-key {:a "a" :b "b"} :c "c"}})
               {:session {:c "c"}})))
     (testing "destroy-session! works when combined with wrap-flash"
        (is (= ((wrap-stateful-session*
                 (ring.middleware.flash/wrap-flash
                  (fn [r] (do (destroy-session!)
                              {}))))
                {:session {session-key {:a "a" :b "b"}}})
               {:session nil})))
     (testing "response values DO NOT override existing session values"
        (is (= ((wrap-stateful-session*
                 (fn [r] (do (session-put! :a "b")
                             {:session {:a "c"}})))
                {:session {:a "a"}})
               {:session {session-key {:a "b"} :a "c"}})))
     (testing "no session in request or response or stateful-session"
        (is (= ((wrap-stateful-session*
                 (fn [r] {}))
                {})
               {})))
     (testing "stateful-sesssion should return nil when the handler returns nil"
        (is (= ((wrap-stateful-session*
                 (fn [r] nil)) {:session {}})
               nil)))))

(deftest wrap-stateful-session-test
  (let [session-map (atom {})
        store (memory-store session-map)
        handler (fn [r] (do (session-put! :x 1)
                            {:status 200 :body "hello"}))
        handler (wrap-stateful-session handler {:store store})
        session-key-name :ring-session-key-name
        result (handler {:cookies {"ring-session" {:value session-key-name}}})
        [_ stateful-map] (first @session-map)]
    (is (= (:body result) "hello"))
    (is (= (-> stateful-map session-key :x) 1))))


(deftest update-session!-test
  (binding [*session* (atom {})]
    (is (= (:t (update-session! (fn [a b] (assoc a :t b)) "t"))
           "t"))))

(deftest session-put!-test
  (binding [*session* (atom {})]
    (let [r (session-put! :t "t")]
      (is (= (session-get :t) "t")))))

(deftest session-get-test
  (binding [*session* (atom {})]
    (let [r (session-put! :t "t")]
      (is (= (session-get :t) "t")))))

(deftest flash-put!-test
  (binding [*flash* (atom {})]
    (let [r (flash-put! :t "t")]
      (is (= (flash-get :t) "t")))))

(deftest flash-get-test
  (binding [*flash* (atom {})]
    (let [r (flash-put! :t "t")]
      (is (= (flash-get :t) "t")))))



