;;; mg-vcs ---  Summary: For interfacing with kanban, GTD, Jira, and other scheduling things
;;; -*- lexical-binding: t -*-

;;; Code: 
(use-package magit-todos
  :ensure t
  :after magit
  :config (magit-todos-mode 1))

(use-package forge
  :after magit)
