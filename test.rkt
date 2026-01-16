#lang racket

(require "gen-utils.rkt")

(define cc (date 0 0 0 9 1 2026 0 0 #f 0))

(date->dd/mm/yyyy cc)

