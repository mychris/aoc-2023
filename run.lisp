(ql:quickload "aoc-2023" :verbose nil :silent t)

(cl:princ (format nil "Advent of Code 2023~&"))

(cl:princ (format nil "Day 2: Trebuchet?! ~S~&"
                  (cons (aoc-2023:trebuchet-1) (aoc-2023:trebuchet-2))))

(cl:princ (format nil "Day 2: Cube Conundrum ~S~&"
                  (cons (aoc-2023:cube-conundrum-1) (aoc-2023:cube-conundrum-2))))

(cl:princ (format nil "Day 3: Gear Ratios ~S~&"
                  (cons (aoc-2023:gear-ratios-1) (aoc-2023:gear-ratios-2))))
