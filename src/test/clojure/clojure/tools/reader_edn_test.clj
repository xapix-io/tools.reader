(ns clojure.tools.reader-edn-test
  (:refer-clojure :exclude [read-string read])
  (:require [clojure.tools.reader.reader-types :refer [indexing-push-back-reader]]
            [clojure.tools.reader.edn :refer [read read-string]]
            [clojure.test :refer [deftest is]]
            [clojure.walk :refer [prewalk]])
  (:import clojure.lang.BigInt))

(load "common_tests")

(deftest read-keyword
  (is (= :foo-bar (read-string ":foo-bar")))
  (is (= :foo/bar (read-string ":foo/bar")))
  (is (= :*+!-_? (read-string ":*+!-_?")))
  (is (= :abc:def:ghi (read-string ":abc:def:ghi")))
  (is (= :abc.def/ghi (read-string ":abc.def/ghi")))
  (is (= :abc/def.ghi (read-string ":abc/def.ghi")))
  (is (= :abc:def/ghi:jkl.mno (read-string ":abc:def/ghi:jkl.mno")))
  (is (instance? clojure.lang.Keyword (read-string ":alphabet"))) )

(deftest read-tagged
  ;; (is (= #inst "2010-11-12T13:14:15.666"
  ;;        (read-string "#inst \"2010-11-12T13:14:15.666\"")))
  ;; (is (= #inst "2010-11-12T13:14:15.666"
  ;;        (read-string "#inst\"2010-11-12T13:14:15.666\"")))
  ;; (is (= #uuid "550e8400-e29b-41d4-a716-446655440000"
  ;;        (read-string "#uuid \"550e8400-e29b-41d4-a716-446655440000\"")))
  ;; (is (= #uuid "550e8400-e29b-41d4-a716-446655440000"
  ;;        (read-string "#uuid\"550e8400-e29b-41d4-a716-446655440000\"")))
  (is (= (java.util.UUID/fromString "550e8400-e29b-41d4-a716-446655440000")
         (read-string "#uuid \"550e8400-e29b-41d4-a716-446655440000\"")))
  (is (= (java.util.UUID/fromString "550e8400-e29b-41d4-a716-446655440000")
         (read-string "#uuid\"550e8400-e29b-41d4-a716-446655440000\"")))
  (let [my-unknown (fn [tag val] {:unknown-tag tag :value val})]
    (is (= {:unknown-tag 'foo :value 'bar}
           (read-string {:default my-unknown} "#foo bar")))))

(deftest read-source-position
  (letfn [(edn-read [st]
            (read {:source-position true :eof nil}
                  (indexing-push-back-reader st)))]
    (is (= {:line 1 :column 1 :name "foo" :ns nil :file nil}
           (meta (edn-read "foo"))))

    (is (= {:line 1 :column 3 :name "foo" :ns nil :file nil}
           (meta (edn-read "  foo  "))))

    (is (= [{:line 1 :column 2 :file nil :name "bar" :ns "foo"}
            {:line 1 :column 11 :file nil :name "fn" :ns nil}
            {:line 1 :column 15 :file nil :name "x" :ns nil}
            {:line 2 :column 47 :file nil :name "+" :ns nil}]
           (let [all-meta (atom [])]
             (prewalk (fn [x]
                        (when (symbol? x)
                          (swap! all-meta conj (meta x)))
                        x)
                      (edn-read "{foo/bar (fn [x]
                                             (+ 1 1))}"))
             @all-meta)))))
