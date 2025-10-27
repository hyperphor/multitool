(ns org.candelbio.multitool.web
  (:require [clojure.string :as str]
            [org.candelbio.multitool.core :as u]
            )
  )

;;; Status: collecting stuff, needs cleaning etc.

;;; TODO use cemerick library
;;; TODO → multitool, also find a better one
;;; TODO doesn't even handle #foo?bar stuff, so pretty useess
;;; Clj only because of some weird regex issue, sigh
;;; Requires a protocol
#?(:clj
   (defn parse-url
     [url]
     (let [[match? protocol host rest] (re-matches #"^(\w+)\:\/\/(.+?)(\/.*)?" url)]
       (when match? [protocol host rest]))))


#?(:clj
   (def url-regex #"https?:\/\/[-a-zA-Z0-9@:%._\+~#=]{1,256}\.[a-zA-Z0-9()]{1,6}\b[-a-zA-Z0-9@:%_\+.~#?&//=]*")
   :cljs
   (def url-regex   #"http(s)?:.*")
   )

#?(:clj

;;; From voracious
   (defn url?
     [url]
     (and url (string? url) (re-matches url-regex url)))

   :cljs
   (defn url?
     [s]
     (and (string? s)
          (boolean
           (re-matches url-regex s))))
   )


#?(:clj
(defn url-base-host
  [url-or-host]
  (let [host (if (url? url-or-host)
               (second (parse-url url-or-host))
               url-or-host)
        comps (str/split host #"\.")]
    (str/join "." (take-last 2 comps)))) ;Not always right, but close enough for now...
   )

(defn find-urls
  [s]
  (re-seq url-regex s))

(defn find-urls-struct
  [struct]
  (-> (u/walk-collect
       #(when (string? %)
          (find-urls %))
       struct)
      flatten
      set))

#?(:cljs
   (defn url-decode
     [s]
     (js/decodeURIComponent s))

   :clj
   (defn url-decode
     [s]
     (java.net.URLDecoder/decode s "UTF-8"))
   )

#?(:cljs
   (defn url-encode
     [s]
     (js/encodeURIComponent s))

   ;; NOTE: this turns spacesx to + which js has a bard time with, so prob fix that.
   :clj
   (defn url-encode
     [s]
     (-> (java.net.URLEncoder/encode s "UTF-8")
         (str/replace "+" "%20")))        ;insane that you have to do this, but if fact java and js have INCOMPATIBLE encode/decode fns
   )
