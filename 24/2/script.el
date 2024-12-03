;;; day1.el --- Advent of Code 2024-Day2  -*- lexical-binding: t -*-
;; Copyright (C) 2024
;; SPDX-License-Identifier: MIT
;; Maintainer: bdosorio
;; Keywords: advent-of-code
;; Package: my-aoc-day2

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; Trying out ELISP this year for Advent of Code. This file solves https://adventofcode.com/2024/day/2
;; Using package my as I only intend to use for AoC calculations, not meant to be used directly by others.

(defun my-aoc-day2-read-number-lists-from-lines (filename)
  "Read lines from file and convert into a list of number lists"
  (let (
	(lines '())
	(numberLists '()))
    (with-temp-buffer (insert-file-contents filename)
		      (goto-char (point-min))
		      (while (not (eobp))
			(push (buffer-substring-no-properties (line-beginning-position) (line-end-position)) lines)
			(forward-line 1)))
    (dolist (line (reverse lines))
      (push (seq-map 'string-to-number (split-string line)) numberLists)
      )
    numberLists
    ))

(print (my-aoc-day2-read-number-lists-from-lines "input.txt"))


(defun my-aoc-day2-count-safe (levels)
  "Read levels and determine how many are safe"
  (let (
	(sum 0)
	)
    (dolist (level levels)
      (let (
	    (i 0)
	    (isSafe 't)
	    )
	(while (and (< (1+ i) (length level)) isSafe)
	  (let ((num1 (nth i level))
		(num2 (nth (1+ i) level))
		)
	    (setq delta (abs (- num1 num2)))
	    (unless (or (< delta 1) (> delta 3))
		(setq isSafe nil))
	    (setq i (1+ i)))	
	  )
        (when isSafe
	  (setq sum (1+ sum))
	)))
      sum
      ))

;; tests

(ert-deftest my-aoc-day2-count-safe-no-input ()
  (should (= (my-aoc-day2-count-safe (list '() )) 0))
  )

(ert-deftest my-aoc-day2-count-safe-one-line-ascending-is-safe ()
  (should (= (my-aoc-day1-part1 (list '(1 2 3) )) 1))
  )

(ert-deftest my-aoc-day2-count-safe-one-line-big-increase-is-unsafe ()
  (should (= (my-aoc-day1-part1 (list '(1 2 7 8 9) )) 0))
  )

(ert-deftest my-aoc-day2-part2-puzzle-input ()
  (should (= (my-aoc-day2-count-safe (my-aoc-day2-read-number-lists-from-lines "input.txt")) 2057374))
  )

