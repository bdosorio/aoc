;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with <open> and enter text in its buffer.


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
      (while (< i (length column1))
	(let ((num1 (nth i column1)) (num2 (nth i column2)))
	  (setq sum (+ sum (abs (- num1 num2))))
	  )
	(setq i (1+ i)))
      sum
      ))

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


(let (
      (result (my-aoc-read-numbers-from-two-columns "./input.txt")))
  (setq column1 (nth 0 result))
  (setq column2 (nth 1 result))
  (message "Smallest in col1: %s, col2: %s" (car (sort column1 '<)) (car (sort column2 '<)))
  )

