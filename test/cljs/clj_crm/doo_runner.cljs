(ns clj-crm.doo-runner
  (:require [doo.runner :refer-macros [doo-tests]]
            [clj-crm.core-test]))

(doo-tests 'clj-crm.core-test)

