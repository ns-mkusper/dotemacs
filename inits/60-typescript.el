(use-package typescript-mode
  :straight t
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :init
  (setq interpreter-mode-alist (assoc-delete-all "node" interpreter-mode-alist))
  (add-to-list 'magic-mode-alist
               '((lambda ()
                   (string-match "\\.ts$" (buffer-file-name))) . typescript-mode))

  :config
  (setq typescript-indent-level 2)
  ;; Set up Node debugging
  (require 'dap-node)
  (dap-node-setup) ;; Automatically installs Node debug adapter if needed
  (dap-register-debug-template "node::launch::debug_current_file"
                               (list :type "node"
                                     :request "launch"
                                     :smartStep t
                                     :cwd "${workspaceFolder}"
                                     :outFiles ["${workspaceFolder}/**/*.js"]
                                     :skipFiles ["<node_internals>/**"]
                                     :program "${file}"
                                     ;; Or
                                     ;; :program "${workspaceFolder}//APP.ts" ; replace with the filename & path to debug
                                     ;; :args (list "")                       ; uncomment & replace with the arguments to program
                                     ))
  (dap-register-debug-template "node::launch::npm"
                               (list :type "node"
                                     :request "launch"
                                     :smartStep t
                                     :cwd "${workspaceFolder}"
                                     :outFiles ["${workspaceFolder}/**/*.js"]
                                     :skipFiles ["<node_internals>/**"]
                                     :runtimeExecutable "npm"
                                     :runtimeArgs ["run-script", "test"]
                                     ))
  (dap-register-debug-template "node::launch::yarn"
                               (list :type "node"
                                     :request "launch"
                                     :smartStep t
                                     :cwd "${workspaceFolder}"
                                     :outFiles ["${workspaceFolder}/**/*.js"]
                                     :skipFiles ["<node_internals>/**"]
                                     :runtimeExecutable "yarn"
                                     :runtimeArgs ["test"]
                                     ))
  )

(provide '60-typescript)
