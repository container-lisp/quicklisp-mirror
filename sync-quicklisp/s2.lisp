(ql:quickload :quicklisp-mirror-tool)

(in-package :quicklisp-mirror-tool.dist)

(initialize/dist)
(mirror/dist)
(mirror/releases)

(sb-ext:quit)


