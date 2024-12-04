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

(defun my-aoc-day2-count-safe (levels)
  "Read levels and determine how many are safe"
  (let ((sum 0))
    (dolist (level levels)
      (let (
	    (i 0)
	    (isSafe t)
	    (levelLength (length level))
	    )
	(unless (or (= 0 levelLength) (and (not (apply '< level)) (not (apply '> level))))
	  (while (and (< (1+ i) levelLength) isSafe)
		      (let ((num1 (nth i level))
			    (num2 (nth (1+ i) level))
			    (delta 0))
			(setq delta (abs (- num1 num2)))
			(when (or (< delta 1) (> delta 3))
			  (setq isSafe nil))
			(setq i (1+ i))))	
          (when isSafe
	      (setq sum (1+ sum))
	      ))))
      sum
      ))

;; tests

(ert-deftest my-aoc-day2-count-safe-no-input ()
  (should (= (my-aoc-day2-count-safe (list '() )) 0))
  )

(ert-deftest my-aoc-day2-count-safe-one-line-ascending-is-safe ()
  (should (= (my-aoc-day2-count-safe (list '(1 2 3) )) 1))
  )

(ert-deftest my-aoc-day2-count-safe-one-line-descending-is-safe ()
  (should (= (my-aoc-day2-count-safe (list '(3 2 1) )) 1)) 
  )


(ert-deftest my-aoc-day2-count-safe-one-line-big-increase-is-unsafe ()
  (should (= (my-aoc-day2-count-safe (list '(1 2 7 8 9) )) 0))
  )

(ert-deftest my-aoc-day2-count-safe-one-line-big-decrease-is-unsafe ()
  (should (= (my-aoc-day2-count-safe (list '(9 8 7 2 1) )) 0))
  )

(ert-deftest my-aoc-day2-count-safe-one-line-flip-asc-to-descend-is-unsafe ()
  (should (= (my-aoc-day2-count-safe (list '(1 4 7 5 3) )) 0))
  )

(ert-deftest my-aoc-day2-count-safe-sample-input ()
  (should (= (my-aoc-day2-count-safe (list '(7 6 4 2 1)
                                           '(1 2 7 8 9)
					   '(9 7 6 2 1)
					   '(1 3 2 4 5)
					   '(8 6 4 4 1)
					   '(1 3 6 7 9)
					   )) 2))
  )

(ert-deftest my-aoc-day2-part2-puzzle-input ()
  (should (= (my-aoc-day2-count-safe (my-aoc-day2-read-number-lists-from-lines "input.txt")) 639))
  )

