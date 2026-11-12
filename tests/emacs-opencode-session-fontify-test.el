;;; emacs-opencode-session-fontify-test.el --- Tests for font-lock  -*- lexical-binding: t; -*-

(require 'ert)
(require 'emacs-opencode-session-fontify)

;;; regex constants

(ert-deftest test-opencode-fontify/regex-code-matches-backticks ()
  "Inline code regex matches backtick-delimited code."
  (with-temp-buffer
    (insert "hello `code` world")
    (goto-char (point-min))
    (should (re-search-forward opencode-session--regex-code nil t))
    (should (equal (match-string 3) "code"))))

(ert-deftest test-opencode-fontify/regex-bold-matches-asterisks ()
  "Bold regex matches **bold** text."
  (with-temp-buffer
    (insert "hello **bold** world")
    (goto-char (point-min))
    (should (re-search-forward opencode-session--regex-bold nil t))
    (should (equal (match-string 4) "bold"))))

(ert-deftest test-opencode-fontify/regex-italic-matches-single-asterisk ()
  "Italic regex matches *italic* text."
  (with-temp-buffer
    (insert "hello *italic* world")
    (goto-char (point-min))
    (should (re-search-forward opencode-session--regex-italic nil t))
    (should (equal (match-string 3) "italic"))))

(ert-deftest test-opencode-fontify/regex-bold-italic-matches-triple-asterisks ()
  "Bold-italic regex matches ***bold italic*** text."
  (with-temp-buffer
    (insert "hello ***bold italic*** world")
    (goto-char (point-min))
    (should (re-search-forward opencode-session--regex-bold-italic nil t))
    (should (equal (match-string 4) "bold italic"))))

(ert-deftest test-opencode-fontify/bold-italic-face-exists ()
  "Bold-italic face is defined."
  (should (facep 'opencode-markdown-bold-italic-face)))

;;; Emphasis does not cross newlines

(ert-deftest test-opencode-fontify/bold-does-not-cross-newlines ()
  "Bold regex does not match across newlines."
  (with-temp-buffer
    (insert "hello **bold\ntext** world")
    (goto-char (point-min))
    (should-not (re-search-forward opencode-session--regex-bold nil t))))

(ert-deftest test-opencode-fontify/italic-does-not-cross-newlines ()
  "Italic regex does not match across newlines."
  (with-temp-buffer
    (insert "hello *italic\ntext* world")
    (goto-char (point-min))
    (should-not (re-search-forward opencode-session--regex-italic nil t))))

(ert-deftest test-opencode-fontify/bold-italic-does-not-cross-newlines ()
  "Bold-italic regex does not match across newlines."
  (with-temp-buffer
    (insert "hello ***bold italic\ntext*** world")
    (goto-char (point-min))
    (should-not (re-search-forward opencode-session--regex-bold-italic nil t))))

;;; Bold followed by italic on same line

(ert-deftest test-opencode-fontify/bold-then-italic-separate ()
  "Bold and italic on the same line do not interfere with each other."
  (with-temp-buffer
    (insert (propertize "**bold** and *italic*"
                        'opencode-part-type "assistant-text"))
    ;; Run bold fontifier
    (goto-char (point-min))
    (opencode-session--fontify-bold (point-max))
    ;; Check bold applied correctly
    (should (eq (get-text-property 3 'face) 'opencode-markdown-bold-face))
    ;; Check bold property is set on the delimiter region
    (should (get-text-property 1 'opencode-bold))
    ;; Run italic matcher
    (goto-char (point-min))
    (let ((result (opencode-session--match-italic (point-max))))
      (should result)
      ;; The italic match should be "italic", not "* and *"
      (should (equal (match-string 2) "italic")))))

;;; in-part-p

(ert-deftest test-opencode-fontify/in-part-p ()
  "Check text property for part type."
  (with-temp-buffer
    (insert (propertize "hello" 'opencode-part-type "user-text"))
    (should (opencode-session--in-part-p 1 "user-text"))
    (should-not (opencode-session--in-part-p 1 "tool"))))

(ert-deftest test-opencode-fontify/in-text-part-p ()
  "Check text property for user-text or assistant-text."
  (with-temp-buffer
    (insert (propertize "hello" 'opencode-part-type "assistant-text"))
    (should (opencode-session--in-text-part-p 1))
    (insert (propertize "tool" 'opencode-part-type "tool"))
    (should-not (opencode-session--in-text-part-p (+ 1 5)))))

;;; face-p

(ert-deftest test-opencode-fontify/face-p-single ()
  "Detect a single face."
  (with-temp-buffer
    (insert (propertize "hello" 'face 'bold))
    (should (opencode-session--face-p 1 '(bold)))))

(ert-deftest test-opencode-fontify/face-p-list ()
  "Detect a face in a list of faces."
  (with-temp-buffer
    (insert (propertize "hello" 'face '(bold italic)))
    (should (opencode-session--face-p 1 '(italic)))))

(ert-deftest test-opencode-fontify/face-p-no-match ()
  "Return nil when face doesn't match."
  (with-temp-buffer
    (insert (propertize "hello" 'face 'bold))
    (should-not (opencode-session--face-p 1 '(italic)))))

;;; block separator

(ert-deftest test-opencode-fontify/block-separator-regex ()
  "Block separator matches blank lines."
  (with-temp-buffer
    (insert "line1\n\nline2")
    (goto-char (point-min))
    (should (re-search-forward opencode-session--regex-block-separator nil t))))

;;; table fontification

(ert-deftest test-opencode-fontify/table-separator-line-p ()
  "Detect table separator lines."
  (should (opencode-session--table-separator-line-p "|---|---|"))
  (should (opencode-session--table-separator-line-p "| --- | --- |"))
  (should (opencode-session--table-separator-line-p "|:---|---:|"))
  (should-not (opencode-session--table-separator-line-p "| a | b |"))
  (should-not (opencode-session--table-separator-line-p "not a table")))

(ert-deftest test-opencode-fontify/table-wrapped-header ()
  "Every line above the first separator is a header line."
  (with-temp-buffer
    (insert (propertize (string-join '("| Long  |"
                                       "| title |"
                                       "| ----- |"
                                       "| body  |"
                                       "| ----- |"
                                       "| more  |")
                                     "\n")
                        'opencode-part-type "assistant-text"))
    (goto-char (point-min))
    (opencode-session--fontify-tables (line-end-position))
    (dolist (word '("Long" "title"))
      (goto-char (point-min))
      (search-forward word)
      (should (eq (get-text-property (match-beginning 0) 'face)
                  'opencode-markdown-table-header-face)))
    (dolist (word '("body" "more"))
      (goto-char (point-min))
      (search-forward word)
      (should-not (get-text-property (match-beginning 0) 'face)))))

(ert-deftest test-opencode-fontify/table-faces-exist ()
  "Table faces are defined."
  (should (facep 'opencode-markdown-table-delimiter-face))
  (should (facep 'opencode-markdown-table-separator-face))
  (should (facep 'opencode-markdown-table-header-face)))

(provide 'emacs-opencode-session-fontify-test)

;;; emacs-opencode-session-fontify-test.el ends here
