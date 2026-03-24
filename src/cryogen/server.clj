(ns cryogen.server
  (:require
    [compojure.core :refer [GET defroutes]]
    [compojure.route :as route]
    [ring.util.response :refer [redirect file-response]]
    [ring.util.codec :refer [url-decode]]
    [ring.server.standalone :as ring-server]
    [cryogen-core.watcher :refer [start-watcher! start-watcher-for-changes!]]
    [cryogen-core.plugins :refer [load-plugins]]
    [cryogen-core.compiler :refer [compile-assets-timed]]
    [cryogen-core.config :refer [resolve-config]]
    [cryogen-core.io :refer [path]]
    [cryogen-core.util :as util]
    [clojure.string :as string])
  (:import (java.io File)))

(def resolved-config (delay (resolve-config)))

(defn htmlize
  [{:keys [postprocess-article-html-fn] :as params} post]
  (letfn [(postprocess-article [article]
            (if postprocess-article-html-fn
              (postprocess-article-html-fn article params)
              article))]
    (-> post
        ((fn [{dom :content-dom :as article}]
          (-> article
              (dissoc :content-dom)
              (assoc
                :full (util/enlive->html-text dom)
                :content (util/enlive->html-text (take (params :blocks-per-preview) dom))
                :dom dom))))
              ; NOT WORKING FOR ASCIIDOC >:(
        postprocess-article
        ((fn [article]
          (-> article
              (assoc :preview (article :content))
              (assoc :content (article :full)))))
        postprocess-article
        )))

(defn archive-order
  [posts]
  (->> posts
       (group-by :formatted-archive-group)
       (map (fn [[group postset]]
              {:group group
               :parsed-group (:parsed-archive-group (get posts 0))
               :posts postset}))
       (sort-by :parsed-group)))

(def extra-config-dev
  {:extend-params-fn
   (fn extend-params [params {:keys [posts] :as site-data}]
     (let [tag-count (->> (:posts-by-tag site-data)
                          (map (fn [[k v]] [k (count v)]))
                          (into {}))
           tags-with-count (update
                             params :tags
                             #(map(fn [t] (assoc t
                                            :count (tag-count (:name t)))) %))
           allposts (map (fn [post]
                           (let [html (htmlize params post)]
                             (assoc post 
                                    :full (html :content) 
                                    :preview (html :preview)
                                    :dom (vec (html :dom))
                                    ))) posts)
           ordered (archive-order allposts)
           new-params (assoc tags-with-count
                             :allposts ordered
                             :firstpost (first ((first ordered) :posts)))]
         new-params))})

(defn init [& [fast?]]
  (load-plugins)
  (compile-assets-timed extra-config-dev)
  (let [ignored-files (-> @resolved-config :ignored-files)]
    (run!
      #(if fast?
         (start-watcher-for-changes! % ignored-files compile-assets-timed extra-config-dev)
         (start-watcher! % ignored-files (partial compile-assets-timed extra-config-dev)))
      ["content" "themes"])))

(defn wrap-subdirectories
  [handler]
  (fn [request]
    (let [{:keys [clean-urls blog-prefix public-dest]} @resolved-config
          req-uri (.substring (url-decode (:uri request)) 1)
          res-path (if (or (.endsWith req-uri "/")
                           (.endsWith req-uri ".html")
                           (-> (string/split req-uri #"/")
                               last
                               (string/includes? ".")
                               not))
                     (condp = clean-urls
                       :trailing-slash (path req-uri "index.html")
                       :no-trailing-slash (if (or (= req-uri "")
                                                  (= req-uri "/")
                                                  (= req-uri
                                                     (if (string/blank? blog-prefix)
                                                       blog-prefix
                                                       (.substring blog-prefix 1))))
                                            (path req-uri "index.html")
                                            (path (str req-uri ".html")))
                       :dirty (path (str req-uri ".html")))
                     req-uri)]
        (or (let [rsp (file-response res-path {:root public-dest})
                  body (:body rsp)]
              ;; Add content-type; it cannot be derived from the extension if `:[no-]trailing-slash`
              (cond-> rsp
                      (and body
                           (instance? File body)
                           (string/ends-with? (.getName body) ".html"))
                      (assoc-in [:headers "Content-Type"] "text/html; charset=utf-8")))
            (handler request)))))

(defroutes routes
  (GET "/" [] (redirect (let [config (resolve-config)]
                          (path (:blog-prefix config)
                                (when (= (:clean-urls config) :dirty)
                                  "index.html")))))
  (route/files "/")
  (route/not-found "Page not found"))

(def handler (wrap-subdirectories routes))

(defn serve
  "Entrypoint for running via tools-deps (clojure)"
  [{:keys [fast join?] :as opts}]
  (ring-server/serve
    handler
    (merge
      {:join? (if (some? join?) join? true)
       :init (partial init fast)
       :open-browser? true
       :auto-refresh? fast ; w/o fast it would often try to reload the page before it has been fully compiled
       :refresh-paths [(:public-dest @resolved-config)]}
      opts)))

(defn -main [& args]
  (serve {:port 3000 :fast ((set args) "fast")}))

(comment
  (def srv (serve {:join? false, :fast true}))
  (.stop srv)

  ,)
