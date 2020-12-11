;; -*- lexical-binding: t -*-
(require 'json)
(require 'cl-lib)
(require 'seq)


(defun oxci--make-hash-table()
  (make-hash-table :test #'equal))


(defstruct oxci--project
  initial
  projectroot
  commandfile
  buildroot
  flagmap
  buffers)

(defcustom oxci--build-subdir
  "build"
  "Default build directory name")

(defvar oxci--project-map
  (oxci--make-hash-table)
  "Keeps track of cmake projects")
(clrhash oxci--project-map)


;;;###autoload
(defun ox-cmake-ide()
  (add-hook 'c-mode-hook #'oxci--setup-buffer t)
  (add-hook 'c++-mode-hook #'oxci--setup-buffer t))

(defun oxci--project--setup-irony(project)
  (let ((projectroot (oxci--project-projectroot project)))
    ; remove previous entries about this projectroot
    (setq irony-cdb-json--project-alist
          (seq-filter (lambda (item)
                        (not (string= (car item) projectroot)))
                      irony-cdb-json--project-alist))
    ; add new mapping for project root to compile_commands.json
    (irony-cdb-json-add-compile-commands-path
     (oxci--project-projectroot project)
     (oxci--commandfile-from-project project))))

(defun oxci--create-project(root)
  (let (project)
    (setq project (make-oxci--project
                   :initial t
                   :projectroot root
                   :flagmap (oxci--make-hash-table)
                   :buffers '()))
    (puthash root project oxci--project-map)
    (oxci--project--setup-irony project)
    project))

(defun oxci--project-from-buffer(&optional buffer)
  (unless buffer
    (setq buffer (current-buffer)))
  (let (filename
        filedir
        projectroot
        project)
    (setq filename (buffer-file-name buffer))
    (setq filedir (expand-file-name ".." filename))

    ; try find root
    (setq projectroot (expand-file-name (locate-dominating-file filedir ".git")))
    (when (string-empty-p projectroot)
      (setq projectroot (expand-file-name (locate-dominating-file filedir "build"))))
    (when (string-empty-p projectroot)
      (setq projectroot (expand-file-name (locate-dominating-file filedir "CMakeLists.txt"))))
    (message (format "Project root %s" projectroot))

    (when projectroot
      (setq project (gethash projectroot oxci--project-map))
      (unless project
        (setq project (oxci--create-project projectroot)))
      project)))

(defun oxci--buildroot-from-project(project)
  (let ((buildroot (oxci--project-buildroot project)))
    (unless buildroot
      (setq buildroot (expand-file-name oxci--build-subdir (oxci--project-projectroot project)))
      (setf (oxci--project-buildroot project) buildroot))
    buildroot))

(defun oxci--commandfile-from-project(project)
  (let ((commandfile (oxci--project-commandfile project)))
    (unless commandfile
      (setq commandfile (expand-file-name "compile_commands.json" (oxci--buildroot-from-project project))))
    commandfile))

(defun oxci--run-cmake--finished(project process event)
  (message "oxci: finished cmake process with exit-code %d" (process-exit-status process))
  (let ((flagmap (oxci--project-flagmap project)))
    (clrhash flagmap))
  (oxci--parse-compile-flags project)
  (oxci--apply-flags project))



(defun oxci--get-param-from-flag(param flags)
  (let (result
        keepnext)
    (seq-do
     (lambda (flag)
       (if keepnext
           (progn
             (setq keepnext nil)
             (push flag result))
         (cond ((string-match-p (concat "^" param ".") flag)
                (push (replace-regexp-in-string (concat "^" param) "" flag) result))
               ((string-match-p (concat "^" param) flag)
                (setq keepnext t)))))
     flags)
    result))

(defun oxci--filter-param-from-flag(param flags)
  (let (removenext)
    (seq-filter
     (lambda (flag)
       (if removenext
           (progn
             (setq removenext nil)
             nil)
         (cond ((string-match-p (concat "^" param ".") flag)
                nil)
               ((string-match-p (concat "^" param) flag)
                (setq removenext t)
                nil)
               (t t))))
     flags)))

(defun oxci--get-flags-without-output(flags)
  (oxci--filter-param-from-flag "-o" flags))

(defun oxci--get-defines(flags)
  (oxci--get-param-from-flag "-D" flags))

(defun oxci--get-includes(buildroot flags)
  (seq-map (lambda (path)
             (expand-file-name path buildroot))
           (oxci--get-param-from-flag "-\\(I\\|include\\)" flags)))

(defun oxci--parse-compile-flags(project)
  (let ((commandfile (oxci--commandfile-from-project project))
        (flagmap (oxci--project-flagmap project)))
    (when (file-exists-p commandfile)
      (message (format "oxci: parsing commandfile %s" commandfile))
      (setq json (json-read-file commandfile))
      (seq-doseq (entry json)
        (let ((filename (cdr (assoc 'file entry)))
              (directory (cdr (assoc 'directory entry)))
              (command (cdr (assoc 'command entry)))
              (flags nil))
          (setq flags (split-string-and-unquote (replace-regexp-in-string "\\\\\"" "\"" command)))
          (puthash filename flags flagmap))))))


(defun oxci--apply-flags(&optional project buffer)
  "(Re-)apply compile flags to a buffer"
  (interactive)
  (unless project
    (setq project (oxci--project-from-buffer buffer))
    (setq buffer (current-buffer)))
  (if (not project)
      (oxci--setup-buffer buffer)
    ; parse flags once
    (let ((flagmap (oxci--project-flagmap project)))
      (unless (and flagmap
                   (> (hash-table-count flagmap) 0))
        (setq flagmap (oxci--parse-compile-flags project))
        (setf (oxci--project-flagmap project) flagmap))
      (if (not buffer)
          ; apply flags for every buffer in project
          (seq-map (lambda(buffer) (oxci--apply-flags project buffer)) (oxci--project-buffers project))
        ; apply flags to single buffer
        (let ((filename (buffer-file-name buffer))
              (buildroot (oxci--buildroot-from-project project))
              flags
              compiler
              includes
              defines
              gcc-flags)
          (setq flags (gethash filename flagmap))
          (setq compiler (car flags))
          (setq flags (cdr flags))

          (when (featurep 'company)
            (make-local-variable 'company-clang-arguments)
            (setq company-clang-arguments flags))

          (when (featurep 'flycheck)
            (flycheck-irony-setup)))))))


(defun oxci--run-cmake(&optional project)
  "Run cmake for a project or the current buffer's project"
  (interactive)
  (unless project
    (setq project (oxci--project-from-buffer (current-buffer))))
  (let ((projectroot (oxci--project-projectroot project))
        (buildroot (oxci--buildroot-from-project project))
        cmake-process)
    (let ((default-directory buildroot))
      (message (format "oxci: running cmake inside %s" buildroot))
      (setq cmake-process (start-process "oxci-cmake" "*cmake*" "cmake" projectroot "-DCMAKE_EXPORT_COMPILE_COMMANDS=ON"))
        (set-process-sentinel
         cmake-process
         (lambda (process event)
           (oxci--run-cmake--finished project process event))))))

(defun oxci--forget-buffer(buffer)
  (let ((project (oxci--project-from-buffer buffer))
        fileslist)
    (setq fileslist (oxci--project-buffers project))
    (delete buffer fileslist)))

(defun oxci--setup-buffer(&optional buffer)
  "Add a file to a managed project"
  (interactive)
  (unless buffer
    (setq buffer (current-buffer)))
  (let ((project (oxci--project-from-buffer))
        buildroot
        commandfile
        fileslist)
    (when project
      (setq buildroot (oxci--buildroot-from-project project))
      (setq commandfile (oxci--commandfile-from-project project))

      ; add local hook for removing buffer from the list on kill
      (add-hook 'kill-buffer-hook (lambda() (oxci--forget-buffer buffer)) nil t)

      ; add file to project's buffer list
      (setq fileslist (oxci--project-buffers project))
      (add-to-list 'fileslist buffer)
      (setf (oxci--project-buffers project) fileslist)

      (message (format "oxci: added file %s to project at %s" (buffer-file-name buffer) (oxci--project-projectroot project)))
      (if (or (oxci--project-initial project)
              (not (file-exists-p commandfile)))
          (progn (setf (oxci--project-initial project) nil)
                 (oxci--run-cmake project))
        (oxci--apply-flags project buffer)))))


























