(setq
 gc-cons-threshold 402653184 ;; Inicia o garbage collector em 384 MB
 gc-cons-percentage 0.6)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/" ) t)
(package-initialize)

;; Localizações/arquivos personalizadas
(setq
 kass/custom-script-dir "~/.emacs.d/lisp/"
 kass/emacs-custom-file "custom.el"

 custom-file (concat kass/custom-script-dir kass/emacs-custom-file) ;; Arquivo diferenciado para salvar as variáveis customizadas
 )

(add-to-list 'load-path kass/custom-script-dir)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(require 'ifind-mode)
(require 'writeroom-mode)
(require 'simpc-mode)

(add-to-list 'auto-mode-alist '("\\.[hc]\\(pp\\)?\\'" . simpc-mode))

(fido-mode t)          ;; Autocomplete
(fido-vertical-mode t) ;; Alternativas aparecem verticalment
(menu-bar-mode -1)     ;; Remove a barra de menu
(tool-bar-mode -1)     ;; Remove a barra de ferramentas
(scroll-bar-mode -1)   ;; Remove a barra de rolagem
(column-number-mode 1) ;; Exibe (linha, coluna) na barra de status
(blink-cursor-mode -1) ;; Cursos para de piscar
(tooltip-mode -1)      ;; Não lembro
(recentf-mode 1)       ;; Não sei
(savehist-mode 1)      ;; Aparentemente salva o histórico, mas não parece funcionar
(electric-indent-mode -1) ;; Desativa a indentação semiautomática


(setq-default
 ;; indent-tabs-mode nil         ;; Alguma coisa relativa a tabs vs espaços
 display-line-numbers-width 4 ;; Quantidade de dígitos reservados para os números das linhas
 truncate-lines t             ;; Não quebrar linhas por padrão
 )

(setq
 display-line-numbers-type 'relative ;; Números relativos nas linhas
 inhibit-splash-screen 1       ;; Sem tela inicial
 inhibit-startup-screen 1      ;; (Quase) a mesma merda
 scroll-conservatively 10000   ;; Para de pular o cursor quando rola para fora do "campo de visão"
 scroll-margin 3               ;; Borda para rolagem vertical
 initial-scratch-message nil   ;; Não exibe uma mensagem inicial no buffer *scratch*
 recentf-max-saved-items nil   ;; Não sei
 tab-width 4                   ;; Tab são 4 espaços
 c-basic-offset 4              ;; Mesma merda só que diferente
 make-backup-files nil         ;; Não salva arquivos de backup
 scroll-step 1                 ;; Scroll do mouse funcional
 split-height-threshold 15     ;; Tamanho máximo para uma nova "janela"
 compilation-window-height 15  ;; Tamanho da "janela" de compilação
 short-answers t               ;; "y" ou "n" em vez de "yes" ou "no"
 require-final-new-line t      ;; Nova linha no final do arquivo
 tab-always-indent 'complete   ;; indenta OU autocompleta
 visible-bell t                ;; Para de tocar o som irritante
 compilation-save-buffers-predicate 'ignore  ;; Ignora o salvamento dos arquivos ao compilar
 ls-lisp-ignore-case t         ;; Relativo a listagem de arquivos
 ls-lisp-dirs-first t          ;; Idem
 switch-to-buffer-in-dedicated-window 'pop ;; ???
 split-width-threshold nil                 ;; ???
 )

;; TODO Verificar se está configurado corretamente
(add-to-list 'display-buffer-alist
             '("\\*ifind\\*"
               (display-buffer-in-side-window)
               (side . bottom)
               (slot . 0)
               (window-height . 0.3)
	       (dedicated . t)
               (window-parameters
                (no-delete-other-windows . nil))))

(add-to-list 'display-buffer-alist
             '("\\*\\([Hh]elp\\|Command History\\|command-log\\|Backtrace\\|Compile-log\\|Messages\\|Warnings\\|compilation\\)\\*"
               (display-buffer-in-side-window)
               (side . bottom)
               (slot . 0)
	       (dedicated . t)
	       (window-height . 0.3)
               (window-parameters
                (no-delete-other-windows . nil))))

;; UTF-8 em tudo
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(global-display-line-numbers-mode t) ;; Exibe as linhas
(global-hl-line-mode t)              ;; Destaque na linha atual
(global-auto-revert-mode t)          ;; Não pede confirmação para recarregar um arquivo do disco
(size-indication-mode t)             ;; Exibe o tamanho do arquivo

(use-package dired
  :config
  (setq dired-kill-when-opening-new-dired-buffer t))

(use-package evil
  :config
  (setq evil-ex-search-case 'sensisite)
  (setq evil-ex-search-persistent-highlight nil)
  (setq evil-ex-interactive-search-highlight nil)
  (setq evil-kill-on-visual-paste nil)
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)

  :custom
  (setq evil-emacs-state-modes '())
  (evil-set-initial-state 'eshell-mode 'normal)
  (evil-set-initial-state 'term-mode 'normal)
  (evil-set-initial-state 'image-mode 'motion)
  (evil-set-initial-state 'special-mode 'motion)
  (evil-set-initial-state 'pdf-view-mode 'motion)
  (evil-set-initial-state 'org-agenda-mode 'motion)
  (evil-set-initial-state 'compilation-mode 'normal)
  (evil-set-initial-state 'grep-mode 'motion)
  (evil-set-initial-state 'Info-mode 'motion)
  (evil-set-initial-state 'magit--mode 'motion)
  (evil-set-initial-state 'magit-status-mode 'motion)
  (evil-set-initial-state 'magit-diff-mode 'motion)
  (evil-set-initial-state 'magit-stashes-mode 'motion)
  (evil-set-initial-state 'epa-key-list-mode 'motion))

(evil-mode 1)
(evil-select-search-module 'evil-search-module 'evil-search)

(modify-syntax-entry ?_ "w")
(add-hook 'c-mode-common-hook
          (lambda () (modify-syntax-entry ?_ "w")))

(add-hook 'html-mode-hook
          (lambda () (setq tab-width 2)))


(global-set-key (kbd "S-<f9>") 'compile)    ;; Compila (sem precisar de sessão)
(global-set-key (kbd "<f9>") 'kass/compile) ;; Compila
(global-set-key (kbd "<f6>") 'dired)   ;; Abre um diretório

;; C-z é esquisito então eu desabilito no modo evil
(define-key evil-insert-state-map   (kbd "C-z") nil)
(define-key evil-motion-state-map   (kbd "C-z") nil)
(define-key evil-visual-state-map   (kbd "C-z") nil)
(define-key evil-replace-state-map  (kbd "C-z") nil)
(define-key evil-operator-state-map (kbd "C-z") nil)
;; E no emacs
(global-set-key (kbd "C-z") nil)

(evil-ex-define-cmd "W" 'evil-write)     ;; w ou W salvam
(evil-ex-define-cmd "E" 'evil-edit)      ;; e ou E editam
(evil-ex-define-cmd "Q" 'evil-quit)      ;; q ou Q fecham

;; Comandos personalizados do Evil
(evil-define-operator kass/evil-align (begin end regex)
  (interactive "<r><a>")
  (align-regexp begin end (concat "\\(\\s-*\\)" (regexp-quote regex))))

(evil-ex-define-cmd "Goyo" 'writeroom-mode)        ;; inicia o "writeroom"
;; TODO Verificar um modo de passar o diretório diretamente no comando, dado que essa chamada
;;  abre o modo interativo do Emacs. O maior problema é que o o modo interativo Evil não
;;  aceita diretórios como parâmetros. Uma ideia é aceitar um arquivo('<f>') e tentar
;;  filtrar somente os diretórios, mas é necessário validar se é possível fazer isto.
(evil-ex-define-cmd "SessionCd" 'kass/session-cd)  ;; modo de "sessão"
(evil-ex-define-cmd "Align" 'kass/evil-align)      ;; align-regexp mas como comando evil

(evil-define-minor-mode-key 'normal 'writeroom-mode (kbd "<up>") 'evil-previous-visual-line)
(evil-define-minor-mode-key 'normal 'writeroom-mode (kbd "<down>") 'evil-next-visual-line)

;; Trocar de janela com as setas do mouse
(evil-define-key '(normal motion) 'global (kbd "C-w <up>") 'evil-window-up)
(evil-define-key '(normal motion) 'global (kbd "C-w <down>") 'evil-window-down)
(evil-define-key '(normal motion) 'global (kbd "C-w <left>") 'evil-window-left)
(evil-define-key '(normal motion) 'global (kbd "C-w <right>") 'evil-window-right)

;; Comentários
(evil-define-key 'normal 'global (kbd "C-/") 'comment-line)
(evil-define-key 'visual 'global (kbd "C-/") 'comment-or-uncomment-region)

;; Shift para mantém a seleção visual
(evil-define-key 'visual 'global (kbd "<") '(lambda ()
                                              (interactive)
                                              (evil-shift-left (region-beginning) (region-end))
                                              (evil-normal-state)
                                              (evil-visual-restore)))

(evil-define-key 'visual 'global (kbd ">") '(lambda ()
                                              (interactive)
                                              (evil-shift-right (region-beginning) (region-end))
                                              (evil-normal-state)
                                              (evil-visual-restore)))

;; Movimentação em com hjkl no modo de inserção
(evil-define-key 'insert 'global (kbd "M-k") 'evil-previous-line)
(evil-define-key 'insert 'global (kbd "M-j") 'evil-next-line)
(evil-define-key 'insert 'global (kbd "M-l") 'evil-forward-char)
(evil-define-key 'insert 'global (kbd "M-h") 'evil-backward-char)

;; Para manter a consistência com a parte acima
(evil-define-key 'insert 'global (kbd "M-<up>")    'evil-previous-line)
(evil-define-key 'insert 'global (kbd "M-<down>")  'evil-next-line)
(evil-define-key 'insert 'global (kbd "M-<right>") 'evil-forward-char)
(evil-define-key 'insert 'global (kbd "M-<left>")  'evil-backward-char)

(evil-set-leader 'normal (kbd "<SPC>"))
(evil-set-leader 'motion (kbd "<SPC>"))
(evil-set-leader 'visual (kbd "<SPC>"))

;; Abrir o .emacs.el
(evil-define-key '(normal motion) 'global (kbd "<leader>s") '(lambda ()
							       (interactive)
							       (select-window (split-window-right))
							       (find-file "~/.emacs.el")))

;; Funções personalizadas

(defun kass/check-session ()
  (if (not (boundp 'kass/session-dir))
      (error "Sessão não iniciada")))

(defun kass/file-inside-dir-p (fname dir)
  (string-prefix-p (expand-file-name dir) fname))

(defun kass/current-buffer-inside-session-dir-p ()
  (interactive)
  (if (boundp 'kass/session-dir)
      (kass/file-inside-dir-p buffer-file-name kass/session-dir)
    nil))

(defun kass/compile ()
  (interactive)
  (kass/check-session)
  (save-some-buffers t 'kass/current-buffer-inside-session-dir-p)
  (call-interactively 'compile))

(defun kass/quote-region (beg end)
  (let ((str (buffer-substring beg end)))
    (replace-regexp-in-region (regexp-quote str) "“\\\&”" beg end)))

(defun kass/quote-current-region ()
  (interactive)
  (kass/quote-region (region-beginning) (region-end)))

;; TODO Verificar se o call-interactively é realmente necessário
(defun kass/quote-current-word ()
  (interactive)
  (evil-visual-state)
  (call-interactively 'evil-inner-WORD)
  (kass/quote-region (region-beginning) (region-end)))

(defun kass/session-cd (folder)
  (interactive (list
                (read-directory-name "Pasta para a sessão: ")))
  (setq default-directory folder)
  (setq-default kass/session-dir folder))

(defun kass/session-dir-or-default ()
  (if (boundp 'kass/session-dir)
      kass/session-dir
    default-directory))

(add-hook 'find-file-hook
          (lambda ()
            (setq default-directory (kass/session-dir-or-default))))

(defun kass/load-custom-file (name)
  (load-file (concat kass/custom-script-dir name)))

(kass/load-custom-file kass/emacs-custom-file) ;; Carrega o arquivo com as variáveis customizadas pelo Emacs

(defun kass/-duplicate-and-comment-line (beg end)
  (interactive)
  (evil-yank-line beg end)
  (evil-set-marker ?a)
  (evil-normal-state)
  (evil-end-of-line)
  (evil-with-single-undo
    (evil-paste-after 1)
    (evil-goto-mark ?\[)
    (evil-visual-char)
    (evil-goto-mark ?\])
    (comment-or-uncomment-region (region-beginning) (region-end))
    (evil-exit-visual-state)
    )
  (evil-goto-mark ?a))

;; TODO Ao desfazer a ação, o cursor não volta à posição original
;; TODO Não parece funcionar em todos os major modes
(defun kass/-duplicate-and-comment-region (beg end)
  (interactive)
  (evil-exit-visual-state)
  (evil-set-marker ?r)
  (evil-yank beg end)
  (goto-char end)
  (evil-beginning-of-line)
  (evil-with-single-undo
    (evil-paste-before 1)
    (evil-goto-mark ?\[)
    (evil-visual-line)
    (evil-goto-mark ?\])
    (comment-or-uncomment-region (region-beginning) (region-end))
    (evil-exit-visual-state)
    )
  (evil-goto-mark ?r))

(defun kass/ifind ()
  "Inicia o modo de busca de arquivos."
  (interactive)
  (kass/check-session)
  (cond ((eq evil-state 'insert) (evil-normal-state))
        ((eq evil-state 'visual) (evil-exit-visual-state)))
  (progn
    (setq default-directory (kass/session-dir-or-default))
    (ifind-mode)))

(defun kass/-save-some-buffers ()
  (interactive)
  (save-some-buffers t))

(defun kass/duplicate-and-comment ()
  "Duplica a linha/região e comente a cópia.

TODO: Não funciona em todos os modos, principalmente quando utilizado no modo visual"
  (interactive)
  (cond ((eq evil-state 'normal)
         (evil-visual-state)
         (kass/-duplicate-and-comment (region-beginning) (region-end)))
        ((eq evil-state 'visual)
         (kass/-duplicate-and-comment-region (region-beginning) (region-end)))))

(evil-define-key 'normal 'global (kbd "C-d") 'kass/duplicate-and-comment)
(evil-define-key 'visual 'global (kbd "C-d") 'kass/duplicate-and-comment)
(evil-define-key '(normal insert visual replace motion) 'global (kbd "C-p") nil) ;; Nem sei o que isso fazia antes

(evil-define-key 'normal 'global (kbd "C-k q") 'kass/quote-current-word)
(evil-define-key 'visual 'global (kbd "C-k q") 'kass/quote-current-region)

(global-set-key (kbd "C-k") nil)      ;; O comportamento padrão é chamar o "kill-line"

(global-set-key (kbd "C-k p") 'kass/ifind)      ;; Encontrar arquivos do projeto
(global-set-key (kbd "C-p")   'kass/ifind)      ;; Mesmo atalho do Neovim
(global-set-key (kbd "C-k c") 'kass/session-cd) ;; Trocar de sessão
(global-set-key (kbd "<f5>")  'eval-buffer)     ;; Recarregar o arquivo atual

(add-hook 'focus-out-hook 'kass/-save-some-buffers)

;; TODO Dependendo do tema a espessura da fonte deveria ser diferente, mas especificação não leva isso em consideração
(set-frame-font "Iosevka Custom Semi-Extended-12" t t)

(setq
 gc-cons-threshold 104857600 ;; Inicia o garbage collector em 100MB
 gc-cons-percentage 0.1)
