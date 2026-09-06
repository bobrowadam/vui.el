;;; vui-stream-node-test.el --- random-access vui-stream nodes -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;;; Commentary:

;; Correctness invariants for the random-access node API (issue #82, the
;; design note docs/design/vui-stream-nodes.org).  `vui-stream-open'
;; returns a stable ref to one buffer region; that ref can be grown
;; (`vui-stream-append-to'), rewritten (`vui-stream-update'), or frozen
;; (`vui-stream-finalize') regardless of how many items land below it.
;; The bar is the same as the rest of vui-stream:
;;
;;   - after any sequence of node ops the buffer is BYTE-IDENTICAL to what
;;     a plain list of the same final items renders (the oracle);
;;   - updating or appending to a node that is NOT the last item is
;;     correct - items below it just shift;
;;   - the model (items-rev) stays in sync, so a forced wholesale
;;     re-render re-emits the streamed content byte-identically;
;;   - `vui-stream-finalize' actually releases the node's markers (the
;;     load-bearing invariant: a finalized node costs the buffer nothing,
;;     so append stays O(1) - see the design note's measurements).

;;; Code:

(require 'buttercup)
(require 'vui)

;; A stream above a declarative note line, same shape as vui-stream-test.
(vui-defcomponent vui-snode-app (stream note)
  :render
  (vui-vstack
   (vui-stream stream)
   (vui-text (format "note: %s" (or note "")))))

(defun vui-snode--oracle (lines note)
  "Expected buffer string: LINES stacked above the NOTE line."
  (if lines
      (concat (mapconcat #'identity lines "\n") "\nnote: " note)
    (concat "note: " note)))

(defun vui-snode--mount (stream note)
  (vui-mount (vui-component 'vui-snode-app :stream stream :note note)
             "*vui-snode*"))

(defun vui-snode--buffer ()
  (with-current-buffer "*vui-snode*" (buffer-string)))

(defun vui-snode--kill ()
  (when (get-buffer "*vui-snode*") (kill-buffer "*vui-snode*")))

(defun vui-snode--empty-stream-state (stream)
  "Capture STREAM's empty-state ownership and rendered layout."
  (let ((start (vui-stream-handle-region-start stream))
        (end (vui-stream-handle-region-end stream))
        (last-start (vui-stream-handle-last-start stream)))
    (list :items (vui-stream-handle-items-rev stream)
          :nodes (vui-stream-handle-nodes stream)
          :inline (with-current-buffer "*vui-snode*"
                    vui--inline-instances)
          :start start :start-position (marker-position start)
          :end end :end-position (marker-position end)
          :last-start last-start
          :last-start-position (and last-start (marker-position last-start))
          :text (vui-snode--buffer))))

(defun vui-snode--expect-empty-stream-state (stream state)
  "Assert STREAM still has the captured empty STATE."
  (expect (vui-stream-handle-items-rev stream) :to-be (plist-get state :items))
  (expect (vui-stream-handle-nodes stream) :to-equal (plist-get state :nodes))
  (expect (with-current-buffer "*vui-snode*" vui--inline-instances)
          :to-equal (plist-get state :inline))
  (expect (vui-stream-handle-region-start stream) :to-be (plist-get state :start))
  (expect (marker-position (vui-stream-handle-region-start stream))
          :to-equal (plist-get state :start-position))
  (expect (vui-stream-handle-region-end stream) :to-be (plist-get state :end))
  (expect (marker-position (vui-stream-handle-region-end stream))
          :to-equal (plist-get state :end-position))
  (expect (vui-stream-handle-last-start stream) :to-be (plist-get state :last-start))
  (expect (and (vui-stream-handle-last-start stream)
               (marker-position (vui-stream-handle-last-start stream)))
          :to-equal (plist-get state :last-start-position))
  (expect (vui-snode--buffer) :to-equal (plist-get state :text)))

(describe "vui-stream nodes: streaming into one ref"
  (it "append-to grows a node, byte-identical to the accumulated text"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-snode--mount s "n0")
      (unwind-protect
          (progn
            ;; non-empty first (open requires a live stream, like a row)
            (vui-stream-append s (vui-text "user: hi"))
            (let ((node (vui-stream-open s (vui-text "reply"))))
              (vui-stream-append-to node (vui-text " streaming"))
              (vui-stream-append-to node (vui-text " in"))
              (expect (vui-snode--buffer)
                      :to-equal
                      (vui-snode--oracle '("user: hi" "reply streaming in") "n0"))))
        (vui-snode--kill))))

  (it "append-to to a NON-last node lands in that node's region"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-snode--mount s "n0")
      (unwind-protect
          (let (a)
            (vui-stream-append s (vui-text "header"))
            (setq a (vui-stream-open s (vui-text "first")))
            (vui-stream-open s (vui-text "second"))   ; a is no longer last
            (vui-stream-append-to a (vui-text "-grown"))
            (expect (vui-snode--buffer)
                    :to-equal
                    (vui-snode--oracle '("header" "first-grown" "second") "n0")))
        (vui-snode--kill)))))

(describe "vui-stream nodes: out-of-order update"
  (it "updates a non-last node, grow then shrink, byte-identical each time"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-snode--mount s "n0")
      (unwind-protect
          (let (a)
            (vui-stream-append s (vui-text "header"))
            (setq a (vui-stream-open s (vui-text "mid")))
            (vui-stream-open s (vui-text "last"))
            ;; grow the middle node
            (vui-stream-update a (vui-text "middle-is-now-long"))
            (expect (vui-snode--buffer)
                    :to-equal
                    (vui-snode--oracle '("header" "middle-is-now-long" "last") "n0"))
            ;; shrink it
            (vui-stream-update a (vui-text "m"))
            (expect (vui-snode--buffer)
                    :to-equal
                    (vui-snode--oracle '("header" "m" "last") "n0")))
        (vui-snode--kill))))

  (it "the last node updates correctly too (region-end stays at the true end)"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-snode--mount s "n0")
      (unwind-protect
          (let (last)
            (vui-stream-append s (vui-text "header"))
            (setq last (vui-stream-open s (vui-text "x")))
            (vui-stream-update last (vui-text "x-grown"))
            ;; a plain static append after editing the last node must still
            ;; land at the true end, not inside the node
            (vui-stream-append s (vui-text "after"))
            (expect (vui-snode--buffer)
                    :to-equal
                    (vui-snode--oracle '("header" "x-grown" "after") "n0")))
        (vui-snode--kill)))))

(describe "vui-stream nodes: finalize releases the marker"
  (it "nulls the node's markers and drops it from the live set"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-snode--mount s "n0")
      (unwind-protect
          (let (node)
            (vui-stream-append s (vui-text "header"))
            (setq node (vui-stream-open s (vui-text "done")))
            (expect (length (vui-stream-handle-nodes s)) :to-equal 1)
            (expect (marker-position (vui--stream-node-start node)) :not :to-be nil)
            (vui-stream-finalize node)
            ;; markers released - the load-bearing invariant
            (expect (vui--stream-node-start node) :to-be nil)
            (expect (vui--stream-node-end node) :to-be nil)
            (expect (vui-stream-handle-nodes s) :to-equal nil)
            ;; text stays exactly as rendered
            (expect (vui-snode--buffer)
                    :to-equal (vui-snode--oracle '("header" "done") "n0")))
        (vui-snode--kill))))

  (it "is a no-op to append-to / update a finalized node"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-snode--mount s "n0")
      (unwind-protect
          (let (node)
            (vui-stream-append s (vui-text "header"))
            (setq node (vui-stream-open s (vui-text "frozen")))
            (vui-stream-finalize node)
            (vui-stream-append-to node (vui-text " IGNORED"))
            (vui-stream-update node (vui-text "ALSO IGNORED"))
            (expect (vui-snode--buffer)
                    :to-equal (vui-snode--oracle '("header" "frozen") "n0")))
        (vui-snode--kill)))))

(describe "vui-stream nodes: last-start stays valid for update-last"
  ;; A boundary node op (after/before/remove on the LAST item) must keep the
  ;; handle's last-start marker pointing at the real last item, or a later
  ;; non-extending `vui-stream-update-last' (content path) deletes the wrong
  ;; region.  The fast extend-path uses region-end and hid this.
  (it "after on the last node, then update-last, rewrites the right item"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-snode--mount s "n0")
      (unwind-protect
          (let (a)
            (vui-stream-append s (vui-text "header"))
            (setq a (vui-stream-open s (vui-text "A")))
            (vui-stream-after a (vui-text "X"))          ; header A X (X last)
            (vui-stream-update-last s (vui-text "Z"))    ; replace X, not A
            (expect (vui-snode--buffer)
                    :to-equal (vui-snode--oracle '("header" "A" "Z") "n0")))
        (vui-snode--kill))))

  (it "before on the last node, then update-last, rewrites the right item"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-snode--mount s "n0")
      (unwind-protect
          (let (l)
            (vui-stream-append s (vui-text "header"))
            (setq l (vui-stream-open s (vui-text "L")))
            (vui-stream-before l (vui-text "X"))         ; header X L (L last)
            (vui-stream-update-last s (vui-text "Q"))    ; replace L, not X
            (expect (vui-snode--buffer)
                    :to-equal (vui-snode--oracle '("header" "X" "Q") "n0")))
        (vui-snode--kill))))

  (it "remove of the last node, then update-last, rewrites the new last item"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-snode--mount s "n0")
      (unwind-protect
          (let (b)
            (vui-stream-append s (vui-text "header"))
            (vui-stream-open s (vui-text "A"))
            (setq b (vui-stream-open s (vui-text "B")))
            (vui-stream-remove b)                        ; header A (A last)
            (vui-stream-update-last s (vui-text "Q"))    ; replace A
            (expect (vui-snode--buffer)
                    :to-equal (vui-snode--oracle '("header" "Q") "n0")))
        (vui-snode--kill)))))

(describe "vui-stream nodes: model stays in sync for re-emit"
  ;; The stream-tail patch is always on, so an in-place re-render leaves the
  ;; live region untouched and would exercise nothing.  Re-mounting the handle
  ;; into a fresh buffer renders the stream from scratch (`vui--stream-render'
  ;; over items-rev), genuinely checking the model.
  (it "re-mounting re-emits the accumulated streamed content"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-snode--mount s "n0")
      (unwind-protect
          (let (node)
            (vui-stream-append s (vui-text "header"))
            (setq node (vui-stream-open s (vui-text "re")))
            (vui-stream-append-to node (vui-text "ply"))
            (vui-stream-append-to node (vui-text "-done"))
            (vui-stream-finalize node)
            (vui-snode--kill)
            (vui-snode--mount s "n0")
            (expect (vui-snode--buffer)
                    :to-equal (vui-snode--oracle '("header" "reply-done") "n0")))
        (vui-snode--kill)))))

(describe "vui-stream nodes: out-of-order insert"
  (it "before inserts a live node above another, byte-identical"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-snode--mount s "n0")
      (unwind-protect
          (let (b x)
            (vui-stream-append s (vui-text "header"))
            (vui-stream-open s (vui-text "A"))
            (setq b (vui-stream-open s (vui-text "B")))
            (setq x (vui-stream-before b (vui-text "X")))
            (expect (vui-snode--buffer)
                    :to-equal (vui-snode--oracle '("header" "A" "X" "B") "n0"))
            ;; the inserted node is live - it can keep streaming
            (vui-stream-append-to x (vui-text "-grown"))
            (expect (vui-snode--buffer)
                    :to-equal
                    (vui-snode--oracle '("header" "A" "X-grown" "B") "n0")))
        (vui-snode--kill))))

  (it "before the only node (opened on an empty stream) puts it on top"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-snode--mount s "n0")
      (unwind-protect
          (let (a)
            (setq a (vui-stream-open s (vui-text "A")))   ; first item, via re-render
            (vui-stream-before a (vui-text "X"))
            (expect (vui-snode--buffer)
                    :to-equal (vui-snode--oracle '("X" "A") "n0")))
        (vui-snode--kill))))

  (it "after inserts a live node below another, byte-identical"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-snode--mount s "n0")
      (unwind-protect
          (let (a)
            (vui-stream-append s (vui-text "header"))
            (setq a (vui-stream-open s (vui-text "A")))
            (vui-stream-open s (vui-text "B"))
            (vui-stream-after a (vui-text "X"))   ; between A and B
            (expect (vui-snode--buffer)
                    :to-equal (vui-snode--oracle '("header" "A" "X" "B") "n0")))
        (vui-snode--kill))))

  (it "after the last node keeps region-end at the true end"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-snode--mount s "n0")
      (unwind-protect
          (let (a)
            (vui-stream-append s (vui-text "header"))
            (setq a (vui-stream-open s (vui-text "A")))
            (vui-stream-after a (vui-text "X"))   ; X is now the last item
            ;; a plain append must land after X, not inside the stream
            (vui-stream-append s (vui-text "tail"))
            (expect (vui-snode--buffer)
                    :to-equal (vui-snode--oracle '("header" "A" "X" "tail") "n0")))
        (vui-snode--kill)))))

(describe "vui-stream nodes: remove"
  (it "removes a middle item, byte-identical"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-snode--mount s "n0")
      (unwind-protect
          (let (b)
            (vui-stream-append s (vui-text "header"))
            (vui-stream-open s (vui-text "A"))
            (setq b (vui-stream-open s (vui-text "B")))
            (vui-stream-open s (vui-text "C"))
            (vui-stream-remove b)
            (expect (vui-snode--buffer)
                    :to-equal (vui-snode--oracle '("header" "A" "C") "n0")))
        (vui-snode--kill))))

  (it "removes the last item and a later append lands correctly"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-snode--mount s "n0")
      (unwind-protect
          (let (b)
            (vui-stream-append s (vui-text "header"))
            (vui-stream-open s (vui-text "A"))
            (setq b (vui-stream-open s (vui-text "B")))
            (vui-stream-remove b)
            (vui-stream-append s (vui-text "tail"))
            (expect (vui-snode--buffer)
                    :to-equal (vui-snode--oracle '("header" "A" "tail") "n0")))
        (vui-snode--kill))))

  (it "removes the first item, byte-identical"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-snode--mount s "n0")
      (unwind-protect
          (let (a)
            (setq a (vui-stream-open s (vui-text "A")))   ; first item
            (vui-stream-open s (vui-text "B"))
            (vui-stream-remove a)
            (expect (vui-snode--buffer)
                    :to-equal (vui-snode--oracle '("B") "n0")))
        (vui-snode--kill))))

  (it "removes down to empty, then re-grows"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-snode--mount s "n0")
      (unwind-protect
          (let (a)
            (setq a (vui-stream-open s (vui-text "only")))
            (vui-stream-remove a)
            (expect (vui-snode--buffer) :to-equal (vui-snode--oracle '() "n0"))
            (vui-stream-append s (vui-text "again"))
            (expect (vui-snode--buffer)
                    :to-equal (vui-snode--oracle '("again") "n0")))
        (vui-snode--kill)))))

(describe "vui-stream nodes: out-of-order edits survive a wholesale re-render"
  (it "re-emits insert/remove results in the right order"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-snode--mount s "n0")
      (unwind-protect
          (let (a b)
            (vui-stream-append s (vui-text "header"))
            (setq a (vui-stream-open s (vui-text "A")))
            (setq b (vui-stream-open s (vui-text "B")))
            (vui-stream-after a (vui-text "X"))   ; header A X B
            (vui-stream-remove b)                 ; header A X
            ;; re-mount the handle -> render the model from scratch
            (vui-snode--kill)
            (vui-snode--mount s "n0")
            (expect (vui-snode--buffer)
                    :to-equal (vui-snode--oracle '("header" "A" "X") "n0")))
        (vui-snode--kill)))))

(vui-defcomponent vui-snode-row (label)
  "A trivial row that renders its LABEL, for component-node tests."
  :render (vui-text (format "[%s]" label)))

(vui-defcomponent vui-snode-open-row-during-render (stream)
  "Attempt `vui-stream-open-row' during render for its rejection test."
  :render
  (vui-stream-open-row stream (vui-component 'vui-snode-row :label "bad")))

(vui-defcomponent vui-snode-indented-app (stream)
  "Place STREAM in a layout that cannot preserve inline rows on root refresh."
  :render (vui-vstack :indent 2 (vui-stream stream)))

(vui-defcomponent vui-snode-batched-layout-app (stream)
  "Switch STREAM from an eligible root layout to an ineligible one."
  :state ((indented nil))
  :render
  (if indented
      (vui-vstack :indent 2 (vui-stream stream))
    (vui-vstack (vui-stream stream))))

(vui-defcomponent vui-snode-batch-note ()
  "Expose a state update in a root separate from the stream."
  :state ((label "old"))
  :render (vui-text label))

(defvar vui-snode--failing-cleanup-count 0)

(vui-defcomponent vui-snode-failing-cleanup-row ()
  "Render empty after registering a cleanup that signals."
  :render
  (progn
    (vui-use-effect ()
      (lambda ()
        (cl-incf vui-snode--failing-cleanup-count)
        (error "failing cleanup")))
    (vui-fragment)))

(describe "vui-stream nodes: component nodes (open a row)"
  (it "opens a component vnode as a live row node"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-snode--mount s "n0")
      (unwind-protect
          (let (node)
            (vui-stream-append s (vui-text "header"))
            (setq node (vui-stream-open
                        s (vui-component 'vui-snode-row :label "a")))
            (expect (vui--stream-node-instance node) :not :to-be nil)
            (expect (vui-snode--buffer)
                    :to-equal (vui-snode--oracle '("header" "[a]") "n0")))
        (vui-snode--kill))))

  (it "tracks inline output when rendering leaves point before the row"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-snode--mount s "n0")
      (unwind-protect
          (progn
            (vui-stream-append s (vui-text "header"))
            (let* ((node (vui-stream-open-row
                          s (vui-component 'vui-snode-row :label "old")))
                   (row (vui--stream-node-instance node))
                   (start (vui-instance-region-start row))
                   (end (vui-instance-region-end row))
                   (stream-end (vui-stream-handle-region-end s))
                   (replacement "[replacement]"))
              ;; Keep static text on both sides of the managed row so a stale
              ;; duplicate or an escaped edit is visible in the final string.
              (with-current-buffer "*vui-snode*"
                (let ((inhibit-read-only t)
                      (inhibit-modification-hooks t)
                      (buffer-undo-list t))
                  (goto-char (point-min))
                  (insert "static-before|")
                  (goto-char (point-max))
                  (insert "|static-after")))
              (let ((start-position (marker-position start))
                    (original-copy-marker (symbol-function 'copy-marker))
                    render-end)
                (expect stream-end :to-be end)
                (cl-letf (((symbol-function 'copy-marker)
                           (lambda (&optional marker type)
                             (let ((copy (funcall original-copy-marker
                                                  marker type)))
                               (when (and (null render-end)
                                          type
                                          (equal marker start-position))
                                 (setq render-end copy))
                               copy)))
                          ((symbol-function 'vui--render-instance)
                           (lambda (&rest _)
                             (insert replacement)
                             ;; The captured failure rendered the row at POS,
                             ;; then returned normally with ambient point
                             ;; before POS.
                             (goto-char (- start-position 2)))))
                  (expect (vui-rerender row) :not :to-throw))
                (expect (vui-snode--buffer)
                        :to-equal
                        (concat "static-before|"
                                (vui-snode--oracle
                                 (list "header" replacement) "n0")
                                "|static-after"))
                (expect (vui-instance-region-start row) :to-be start)
                (expect (vui-instance-region-end row) :to-be end)
                (expect (vui-stream-handle-region-end s) :to-be stream-end)
                (expect stream-end :to-be end)
                (expect (marker-position start) :to-equal start-position)
                (expect (marker-position end)
                        :to-equal (+ start-position (length replacement)))
                (expect (<= (marker-position start) (marker-position end))
                        :to-be t)
                (expect render-end :not :to-be nil)
                (expect (marker-position render-end) :to-be nil)
                ;; A normal later update must replace the same range, not
                ;; leave the captured replacement as a stale duplicate.
                (vui-stream-update
                 node (vui-component 'vui-snode-row :label "new"))
                (expect (vui-snode--buffer)
                        :to-equal
                        (concat "static-before|"
                                (vui-snode--oracle '("header" "[new]") "n0")
                                "|static-after")))))
        (vui-snode--kill))))

  (it "rejects an inline instance with a reversed region before mutation"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-snode--mount s "n0")
      (unwind-protect
          (progn
            (vui-stream-append s (vui-text "header"))
            (let* ((node (vui-stream-open-row
                          s (vui-component 'vui-snode-row :label "old")))
                   (row (vui--stream-node-instance node))
                   (start (vui-instance-region-start row))
                   (end (vui-instance-region-end row))
                   (stream-end (vui-stream-handle-region-end s))
                   (end-position (marker-position end))
                   (reversed-position (1+ end-position))
                   (before (vui-snode--buffer)))
              (set-marker start reversed-position)
              (expect (vui-rerender row) :to-throw 'error)
              (expect (vui-snode--buffer) :to-equal before)
              (expect (marker-position start) :to-equal reversed-position)
              (expect (marker-position end) :to-equal end-position)
              (expect (vui-stream-handle-region-end s) :to-be stream-end)
              (expect (marker-position stream-end) :to-equal end-position)))
        (vui-snode--kill))))

  (it "updates a component node OUT OF ORDER (row no longer last)"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-snode--mount s "n0")
      (unwind-protect
          (let (row)
            (vui-stream-append s (vui-text "header"))
            (setq row (vui-stream-open
                       s (vui-component 'vui-snode-row :label "a")))
            (vui-stream-append s (vui-text "below"))   ; row is no longer last
            (vui-stream-update row (vui-component 'vui-snode-row :label "AA"))
            (expect (vui-snode--buffer)
                    :to-equal (vui-snode--oracle '("header" "[AA]" "below") "n0")))
        (vui-snode--kill))))

  (it "finalize stops tracking a component node but leaves the row live"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-snode--mount s "n0")
      (unwind-protect
          (let (row)
            (vui-stream-append s (vui-text "header"))
            (setq row (vui-stream-open
                       s (vui-component 'vui-snode-row :label "a")))
            (expect (length (vui-stream-handle-nodes s)) :to-equal 1)
            (vui-stream-finalize row)
            (expect (vui-stream-handle-nodes s) :to-equal nil)
            (expect (vui-snode--buffer)
                    :to-equal (vui-snode--oracle '("header" "[a]") "n0")))
        (vui-snode--kill))))

  ;; append-to is content-only: on a component node it must be a clean no-op,
  ;; NOT setcar a fragment over the live instance (which would crash a re-emit).
  (it "append-to on a component node is a clean no-op (items-rev stays sane)"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-snode--mount s "n0")
      (unwind-protect
          (let (row)
            (vui-stream-append s (vui-text "header"))
            (setq row (vui-stream-open
                       s (vui-component 'vui-snode-row :label "a")))
            (vui-stream-append-to row (vui-text "IGNORED"))
            ;; the live instance is still the items-rev head, not wrapped
            (expect (vui-instance-p (car (vui-stream-handle-items-rev s)))
                    :to-be t)
            (expect (vui-snode--buffer)
                    :to-equal (vui-snode--oracle '("header" "[a]") "n0")))
        (vui-snode--kill))))

  (it "open-row bootstraps an empty stream into an owned inline row"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-snode--mount s "n0")
      (unwind-protect
          (let ((node (vui-stream-open-row
                       s (vui-component 'vui-snode-row :label "solo"))))
            (expect (vui--inline-p (vui--stream-node-instance node)) :to-be t)
            (expect (vui-snode--buffer)
                    :to-equal (vui-snode--oracle '("[solo]") "n0"))
            ;; The row is stable before `open-row' returns.
            (vui-stream-update node (vui-component 'vui-snode-row :label "updated"))
            (expect (vui-snode--buffer)
                    :to-equal (vui-snode--oracle '("[updated]") "n0")))
        (vui-snode--kill))))

  (it "preserves and updates an empty-stream row after an eligible root refresh"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-snode--mount s "n0")
      (unwind-protect
          (let* ((root (vui-get-instance "*vui-snode*"))
                 (node (vui-stream-open-row
                        s (vui-component 'vui-snode-row :label "before")))
                 (row (vui--stream-node-instance node)))
            (vui-update root (list :stream s :note "n1"))
            (expect (vui--stream-node-instance node) :to-be row)
            (expect (vui-snode--buffer)
                    :to-equal (vui-snode--oracle '("[before]") "n1"))
            (vui-stream-update node (vui-component 'vui-snode-row :label "after"))
            (expect (vui-snode--buffer)
                    :to-equal (vui-snode--oracle '("[after]") "n1")))
        (vui-snode--kill))))

  (it "rejects a stream outside the root stream-tail layout before mutation"
    (let ((vui-render-delay nil)
          (s (vui-make-stream))
          (buffer "*vui-snode-indented*"))
      (vui-mount (vui-component 'vui-snode-indented-app :stream s) buffer)
      (unwind-protect
          (let ((before (with-current-buffer buffer (buffer-string))))
            (expect (vui-stream-open-row
                     s (vui-component 'vui-snode-row :label "bad"))
                    :to-throw 'error)
            (expect (vui-stream-handle-items-rev s) :to-be nil)
            (with-current-buffer buffer
              (expect vui--inline-instances :to-be nil)
              (expect (buffer-string) :to-equal before)))
        (when (get-buffer buffer) (kill-buffer buffer)))))

  (it "replaces a legacy zero-width node without extra separators"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-snode--mount s "n0")
      (unwind-protect
          (progn
            ;; This is the legacy `vui-stream-open' empty-content shape that
            ;; used to force Mentat's mutable-text promotion workaround.
            (vui-stream-open s (vui-text ""))
            (let ((bootstrap-end (vui-stream-handle-region-end s))
                  (node (vui-stream-open-row
                         s (vui-component 'vui-snode-row :label "row"))))
              (expect (marker-buffer bootstrap-end) :to-be nil)
              (vui-stream-append s (vui-text "tail"))
              (vui-update (vui-get-instance "*vui-snode*")
                          (list :stream s :note "n1"))
              (vui-stream-update node (vui-component 'vui-snode-row :label "updated"))
              (expect (vui-snode--buffer)
                      :to-equal
                      (vui-snode--oracle '("[updated]" "tail") "n1"))))
        (vui-snode--kill))))

  (it "releases nonempty transaction and render markers after success and failure"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-defcomponent vui-snode-transaction-error ()
        :render (error "transaction render failed"))
      (vui-snode--mount s "n0")
      (unwind-protect
          (progn
            (vui-stream-append s (vui-text "header"))
            (let ((original (symbol-function 'copy-marker))
                  markers)
              (cl-letf (((symbol-function 'copy-marker)
                         (lambda (&optional marker-or-integer insertion-type)
                           (let ((marker (funcall original marker-or-integer
                                                  insertion-type)))
                             (push marker markers)
                             marker))))
                (vui-stream-open-row s (vui-component 'vui-snode-row :label "ok")))
              (expect (length markers) :to-equal 3)
              (expect (cl-every (lambda (marker) (not (marker-buffer marker)))
                                markers)
                      :to-be t))
            (let ((original (symbol-function 'copy-marker))
                  markers)
              (cl-letf (((symbol-function 'copy-marker)
                         (lambda (&optional marker-or-integer insertion-type)
                           (let ((marker (funcall original marker-or-integer
                                                  insertion-type)))
                             (push marker markers)
                             marker))))
                (expect (vui-stream-open-row
                         s (vui-component 'vui-snode-transaction-error))
                        :to-throw 'error))
              (expect (length markers) :to-equal 3)
              (expect (cl-every (lambda (marker) (not (marker-buffer marker)))
                                markers)
                      :to-be t)))
        (vui-snode--kill))))

  (it "mounts the empty-stream component exactly once until teardown"
    (let ((vui-render-delay nil)
          (s (vui-make-stream))
          (initializations 0)
          (renders 0)
          (mounts 0)
          (effects 0)
          (cleanups 0)
          (unmounts 0))
      (vui-defcomponent vui-snode-single-mount ()
        :state ((count (progn (cl-incf initializations) 0)))
        :on-mount (cl-incf mounts)
        :on-unmount (cl-incf unmounts)
        :render (progn
                  (cl-incf renders)
                  (vui-use-effect ()
                    (cl-incf effects)
                    (lambda () (cl-incf cleanups)))
                  (vui-text (format "[%d]" count))))
      (vui-snode--mount s "n0")
      (unwind-protect
          (let* ((node (vui-stream-open-row
                        s (vui-component 'vui-snode-single-mount)))
                 (row (vui--stream-node-instance node)))
            (expect initializations :to-equal 1)
            (expect renders :to-equal 1)
            (expect mounts :to-equal 1)
            (expect effects :to-equal 1)
            (expect cleanups :to-equal 0)
            (expect unmounts :to-equal 0)
            (vui-stream-update node (vui-component 'vui-snode-single-mount))
            (expect initializations :to-equal 1)
            (expect mounts :to-equal 1)
            (expect cleanups :to-equal 0)
            (expect unmounts :to-equal 0)
            (vui-unmount row)
            (expect cleanups :to-equal 1)
            (expect unmounts :to-equal 1))
        (vui-snode--kill))))

  (it "open-row settles a pending root render before returning"
    (let ((vui-render-delay 60)
          (s (vui-make-stream)))
      (vui-snode--mount s "n0")
      (unwind-protect
          (let ((root (vui-get-instance "*vui-snode*")))
            (vui--schedule-deferred-render-for root)
            (expect (vui-instance-render-timer root) :not :to-be nil)
            (let ((node (vui-stream-open-row
                         s (vui-component 'vui-snode-row :label "ready"))))
              (expect (vui--inline-p (vui--stream-node-instance node)) :to-be t)
              (expect (vui-instance-render-timer root) :to-be nil)
              (expect (vui-snode--buffer)
                      :to-equal (vui-snode--oracle '("[ready]") "n0"))))
        (vui-snode--kill))))

  (it "open-row preserves another root's batch, including nested batches and timers"
    (dolist (nested '(nil t))
      (dolist (vui-render-delay '(nil 60))
        (let* ((s (vui-make-stream))
               (stream-root (vui-snode--mount s "n0"))
               (other-root (vui-mount (vui-component 'vui-snode-batch-note)
                                      "*vui-snode-other*")))
          (unwind-protect
              (progn
                (when vui-render-delay
                  (vui--schedule-deferred-render-for stream-root))
                (with-current-buffer "*vui-snode-other*"
                  (let ((vui--current-instance other-root))
                    (vui-batch
                     (vui-set-state :label "new")
                     (with-current-buffer "*vui-snode*"
                       (if nested
                           (vui-batch
                            (vui-stream-open-row
                             s (vui-component 'vui-snode-row :label "ready")))
                         (vui-stream-open-row
                          s (vui-component 'vui-snode-row :label "ready"))))
                     (expect (buffer-string) :to-equal "old")
                     (expect vui--render-pending-p :to-be t)))
                  (when vui-render-delay
                    (expect (vui-instance-render-timer other-root) :not :to-be nil)
                    (vui-flush-sync))
                  (expect (buffer-string) :to-equal "new"))
                (expect (vui-instance-render-timer stream-root) :to-be nil)
                (expect (vui-snode--buffer)
                        :to-equal (vui-snode--oracle '("[ready]") "n0")))
            (vui-snode--kill)
            (when (get-buffer "*vui-snode-other*")
              (kill-buffer "*vui-snode-other*")))))))

  (it "open-row settles a timerless batch before revalidating its root layout"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (let ((root (vui-mount
                   (vui-component 'vui-snode-batched-layout-app :stream s)
                   "*vui-snode*")))
        (unwind-protect
            (with-current-buffer "*vui-snode*"
              (let ((vui--current-instance root)
                    (vui--root-instance root))
                (vui-batch
                 (vui-set-state :indented t)
                 (expect vui--render-pending-p :to-be t)
                 (let ((state (vui-snode--empty-stream-state s)))
                   (expect (vui-stream-open-row
                            s (vui-component 'vui-snode-row :label "stale"))
                           :to-throw 'error)
                   (expect vui--render-pending-p :to-be nil)
                   (vui-snode--expect-empty-stream-state s state)))))
          (vui-snode--kill)))))

  (it "open-row rejects an invalid vnode without changing the stream"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-snode--mount s "n0")
      (unwind-protect
          (let ((state (vui-snode--empty-stream-state s)))
            (expect (vui-stream-open-row s (vui-text "not a component"))
                    :to-throw 'error)
            (vui-snode--expect-empty-stream-state s state))
        (vui-snode--kill))))

  (it "rolls back initializer and render errors atomically"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-defcomponent vui-snode-init-error ()
        :state ((value (error "initializer failed")))
        :render (vui-text "unreachable"))
      (vui-defcomponent vui-snode-render-error ()
        :render (error "render failed"))
      (vui-snode--mount s "n0")
      (unwind-protect
          (dolist (component '(vui-snode-init-error vui-snode-render-error))
            (let ((state (vui-snode--empty-stream-state s)))
              (expect (vui-stream-open-row s (vui-component component))
                      :to-throw 'error)
              (vui-snode--expect-empty-stream-state s state)))
        (vui-snode--kill))))

  (it "rejects an initially empty component and rolls back"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-defcomponent vui-snode-empty-row ()
        :render (vui-fragment))
      (vui-snode--mount s "n0")
      (unwind-protect
          (let ((state (vui-snode--empty-stream-state s)))
            (expect (vui-stream-open-row s (vui-component 'vui-snode-empty-row))
                    :to-throw 'error)
            (vui-snode--expect-empty-stream-state s state))
        (vui-snode--kill))))

  (it "runs a failing effect cleanup once and leaves no row residue"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (setq vui-snode--failing-cleanup-count 0)
      (vui-snode--mount s "n0")
      (unwind-protect
          (let ((state (vui-snode--empty-stream-state s)))
            (condition-case err
                (vui-stream-open-row s (vui-component 'vui-snode-failing-cleanup-row))
              (error
               (expect (error-message-string err)
                       :to-match "rejects an initially empty component")))
            (expect vui-snode--failing-cleanup-count :to-equal 1)
            (vui-snode--expect-empty-stream-state s state))
        (vui-snode--kill))))

  (it "rolls back a failed row mount after a nonempty stream"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-defcomponent vui-snode-nonempty-render-error ()
        :render (error "nonempty render failed"))
      (vui-snode--mount s "n0")
      (unwind-protect
          (progn
            (vui-stream-append s (vui-text "header"))
            (let ((items (vui-stream-handle-items-rev s))
                  (nodes (vui-stream-handle-nodes s))
                  (inline (with-current-buffer "*vui-snode*" vui--inline-instances))
                  (end (vui-stream-handle-region-end s))
                  (end-position (marker-position (vui-stream-handle-region-end s)))
                  (text (vui-snode--buffer)))
              (expect (vui-stream-open-row
                       s (vui-component 'vui-snode-nonempty-render-error))
                      :to-throw 'error)
              (expect (vui-stream-handle-items-rev s) :to-be items)
              (expect (vui-stream-handle-nodes s) :to-equal nodes)
              (expect (with-current-buffer "*vui-snode*" vui--inline-instances)
                      :to-equal inline)
              (expect (vui-stream-handle-region-end s) :to-be end)
              (expect (marker-position (vui-stream-handle-region-end s))
                      :to-equal end-position)
              (expect (vui-snode--buffer) :to-equal text)))
        (vui-snode--kill))))

  (it "open-row rejects calls during VUI rendering without changing the stream"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (unwind-protect
          (progn
            (expect (vui-mount
                     (vui-component 'vui-snode-open-row-during-render :stream s)
                     "*vui-snode-render-row*")
                    :to-throw 'error)
            (expect (vui-stream-handle-items-rev s) :to-be nil))
        (when (get-buffer "*vui-snode-render-row*")
          (kill-buffer "*vui-snode-render-row*")))))

  ;; `vui-stream-open' intentionally retains its empty-stream fallback
  ;; contract: it does not claim an inline row.
  (it "open of a component on an empty stream does not capture the root"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-snode--mount s "n0")
      (unwind-protect
          (let (node)
            (setq node (vui-stream-open
                        s (vui-component 'vui-snode-row :label "solo")))
            (expect (vui--stream-node-instance node) :to-be nil)
            (vui-stream-update node (vui-component 'vui-snode-row :label "X"))
            (expect (vui-snode--buffer)
                    :to-equal (vui-snode--oracle '("[solo]") "n0")))
        (vui-snode--kill))))

  ;; After finalize the node is inert: update must not setcar a vnode over the
  ;; still-live row instance in items-rev.
  (it "update after finalize on a component node is a clean no-op"
    (let ((vui-render-delay nil)
          (s (vui-make-stream)))
      (vui-snode--mount s "n0")
      (unwind-protect
          (let (row)
            (vui-stream-append s (vui-text "header"))
            (setq row (vui-stream-open
                       s (vui-component 'vui-snode-row :label "a")))
            (vui-stream-finalize row)
            (vui-stream-update row (vui-component 'vui-snode-row :label "ZZ"))
            (expect (vui-instance-p (car (vui-stream-handle-items-rev s)))
                    :to-be t)
            (expect (vui-snode--buffer)
                    :to-equal (vui-snode--oracle '("header" "[a]") "n0")))
        (vui-snode--kill)))))

(provide 'vui-stream-node-test)
;;; vui-stream-node-test.el ends here
