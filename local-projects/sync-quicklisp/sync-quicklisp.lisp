(in-package :sync-quicklisp)

(defun sync-quicklisp ()
  (quicklisp-mirror-tool.dist:initialize/dist)
  (quicklisp-mirror-tool.dist:fetch-quicklisp.txt)
  (quicklisp-mirror-tool.dist:fetch-quicklisp-versions.txt)
  (quicklisp-mirror-tool.dist:initialize/dist)
  (quicklisp-mirror-tool.dist:mirror/dist))


