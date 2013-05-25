;; my-sectrets.el

(unless (load "~/.secrets.gpg" t)
  (warn "Could not load secrets file!"))
(provide 'my-secrets)
