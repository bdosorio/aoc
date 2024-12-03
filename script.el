;;; day1.el --- Advent of Code 2024-Day1  -*- lexical-binding: t -*-
;; Copyright (C) 2024
;; SPDX-License-Identifier: MIT
;; Maintainer: bdosorio
;; Keywords: advent-of-code
;; Package: my

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; Trying out ELISP this year for Advent of Code. This file solves https://adventofcode.com/2024/day/1
;; Using package my as I only intend to use for AoC calculations, not meant to be used directly by others.

(defun my-aoc-read-numbers-from-two-columns (filename)
  "Read numbers from a file with two whitespace delimited columns into two lists"
  (let (
	(lines '())
	(column1 '())
	(column2 '()))
    (with-temp-buffer (insert-file-contents filename)
		      (goto-char (point-min))
		      (while (not (eobp))
			(push (buffer-substring-no-properties (line-beginning-position) (line-end-position)) lines)
			(forward-line 1)))
    (dolist (line (reverse lines))
      (let (
	    (numbers (split-string line)))
	(push (string-to-number (nth 0 numbers)) column1)
	(push (string-to-number (nth 1 numbers)) column2)))
    (list column1 column2)))


(defun my-aoc-day1-part1 (inputTable)
  "Read numbers from both columns, sort the columns ascending, find difference between each column per line, then sum differences"
  (let (
	(column1 (sort (nth 0 inputTable) '<))
	(column2 (sort (nth 1 inputTable) '<))
	(i 0)
	(sum 0)
	)
      ;; this would probably be a good case for reduce, but my elisp is crap
      (while (< i (length column1))
	(let ((num1 (nth i column1)) (num2 (nth i column2)))
	  (setq sum (+ sum (abs (- num1 num2))))
	  )
	(setq i (1+ i)))
      sum
      ))

(defun my-aoc-day1-part2 (inputTable)
  "Read numbers from both columns, find frequency of all numbers in first column in second column, then sum the frequencies"
  (let (
	(column1 (nth 0 inputTable))
	(column2 (nth 1 inputTable))
	(sum 0)
	)
	(dolist (num1 column1)
	  (setq freq (length (seq-filter (lambda (num2) (= num1 num2)) column2)))
	  (setq sum (+ sum (* num1 freq)))
	  )
      sum
      ))

;; tests

(ert-deftest day1-part1-no-input ()
  (should (= (my-aoc-day1-part1 (list '() '() )) 0))
  )

(ert-deftest day1-part1-one-line ()
  (should (= (my-aoc-day1-part1 (list '(4) '(3) )) 1))
  )

(ert-deftest day1-part1-two-line ()
  (should (= (my-aoc-day1-part1 (list '(4 6) '(7 3) )) 2))
  )

(ert-deftest day1-part1-sample-input ()
  (should (= (my-aoc-day1-part1 (list '(3 4 2 1 3 3) '(4 3 5 3 9 3) )) 11))
  )

(ert-deftest day1-part1-puzzle-input ()
  (should (= (my-aoc-day1-part1 (my-aoc-read-numbers-from-two-columns "./input.txt")) 2057374))
  )

(ert-deftest day1-part2-one-line-no-match ()
  (should (= (my-aoc-day1-part2 (list '(4) '(3) )) 0))
  )

(ert-deftest day1-part2-one-line-one-match ()
  (should (= (my-aoc-day1-part2 (list '(4) '(4) )) 4))
  )

(ert-deftest day1-part2-sample-input ()
  (should (= (my-aoc-day1-part2 (list '(3 4 2 1 3 3) '(4 3 5 3 9 3) )) 31))
  )

(ert-deftest day1-part2-puzzle-input ()
  (should (= (my-aoc-day1-part2 (my-aoc-read-numbers-from-two-columns "./input.txt")) 23177084))
  )
