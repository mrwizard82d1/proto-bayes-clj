(set-env!
 :source-paths #{"src" "test"}

 :dependencies '[[adzerk/boot-test "1.1.1" :scope "test"]])

(require '[adzerk.boot-test :refer :all])
