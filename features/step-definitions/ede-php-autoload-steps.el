(Given "^I visit \"\\(.+\\)\" in project \"\\(.+\\)\"$"
  (lambda (file-path project-name)
    (find-file (ede-php-autoload-test-get-project-file-path file-path project-name))
    ))

(Given "^I update the composer file"
       (lambda ()
         (ede-php-autoload-test-set-composer "new")))

(Given "^I refresh the project autoloads"
       (lambda ()
         (call-interactively #'ede-php-autoload-reload-autoloads)))

(Then "^ede-php-autoload-project should exist$"
      (lambda ()
        (should (ede-php-autoload-project-p (ede-current-project)))))

(Then "^ede-php-autoload-project should have \"\\(.+\\)\" as include path"
      (lambda (include-path)
        (should (string= (car (oref (ede-current-project) :include-path)) include-path))))

(Then "^ede-php-autoload-project should have \"\\(.+\\)\" as system include path"
      (lambda (include-path)
        (should (string= (car (oref (ede-current-project) :system-include-path)) include-path))))

(Then "^the class \"\\(.+\\)\" should be detected in \"\\(.+\\)\"$"
      (lambda (class-name file-path)
        (should
         (string=
          (ede-php-autoload-test-get-project-file-path
           file-path
           (ede-php-autoload-test-get-current-project-name))
          (ede-php-autoload-find-class-def-file (ede-current-project) class-name)))))

(Then "^the class \"\\(.+\\)\" should not be detected"
      (lambda (class-name)
        (should-not (ede-php-autoload-find-class-def-file (ede-current-project) class-name))))

(Then "^guessing the class name for \"\\(.+\\)\" should return \"\\(.+\\)\""
      (lambda (file-name class-name)
        (should (string= (ede-php-autoload-get-class-name-for-file
                          (ede-current-project)
                          file-name)
                         class-name))))

(Then "^type completions for query \"\\(.+\\)\" should be:"
      (lambda (query suggestion-table)
        (let ((suggestions (cl-loop for suggestion in (cdr suggestion-table)
                                    collect (car suggestion))))
          (should (equal (ede-php-autoload-complete-type-name (ede-current-project) query)
                         suggestions)))))

(Then "^type completions for query \"\\(.+\\)\" should be nil"
      (lambda (query)
        (should (null (ede-php-autoload-complete-type-name (ede-current-project) query)))))
