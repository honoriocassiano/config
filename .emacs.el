(setq
 gc-cons-threshold 402653184 ;; Teoricamente aumenta a velocidade de inicialização
 gc-cons-percentage 0.6)

(setq kass/custom-script-dir "~/.emacs.d/lisp/")
(setq kass/emacs-custom-file "custom.el")

(setq custom-file (concat kass/custom-script-dir kass/emacs-custom-file)) ;; Arquivo diferenciado para salvar as variáveis customizadas

(ido-mode 1)         ;; Autocomplete
(ido-everywhere 1)   ;; Autocomplete (de novo)
(menu-bar-mode -1)   ;; Remove a barra de menu
(tool-bar-mode -1)   ;; Remove a barra de ferramentas
(scroll-bar-mode -1) ;; Remove a barra de rolagem
(column-number-mode 1) ;; Exibe (linha, coluna) na barra de status
(blink-cursor-mode -1) ;; Cursos para de piscar
(tooltip-mode -1)      ;; Não lembro
(recentf-mode 1)       ;; Não sei
(savehist-mode 1)      ;; Aparentemente salva o histórico, mas não parece funcionar

(setq-default
 indent-tabs-mode nil) ;; Alguma coisa relativa a tabs vs espaços

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
 display-line-numbers-width 4  ;; Quantidade de dígitos reservados para os números das linhas
 split-height-threshold 15     ;; Tamanho máximo para uma nova "janela"
 compilation-window-height 15) ;; Tamanho da "janela" de compilação

(add-to-list 'default-frame-alist '(fullscreen . maximized)) ;; Maximiza a janela após a inicialização

(global-display-line-numbers-mode 1) ;; Exibe as linhas
(global-hl-line-mode 1)              ;; Destaque na linha atual
(global-auto-revert-mode 1)          ;; Não pede confirmação para recarregar um arquivo do disco

(use-package evil

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

;; TODO Dependendo do tema a espessura da fonte deveria ser diferente, mas especificação não leva isso em consideração
(set-frame-font "Iosevka Custom-12" t t)

(setq split-width-threshold nil) ;; Não lembro

(global-set-key (kbd "<f9>") 'compile) ;; Compila
(global-set-key (kbd "<f6>") 'dired)   ;; Abre um diretório
(global-set-key (kbd "C-/") 'comment-line)

;; C-z é esquisito então eu desabilito no modo evil
(define-key evil-insert-state-map (kbd "C-z") nil)
(define-key evil-motion-state-map (kbd "C-z") nil)
(define-key evil-visual-state-map (kbd "C-z") nil)
(define-key evil-replace-state-map (kbd "C-z") nil)
(define-key evil-operator-state-map (kbd "C-z") nil)
;; E no emacs
(global-set-key (kbd "C-z") nil)

(evil-ex-define-cmd "W" 'evil-write)     ;; w ou W salvam
(evil-ex-define-cmd "E" 'evil-edit)      ;; e ou E editam

;; Comandos personalizados do Evil
(evil-ex-define-cmd "Goyo" 'writeroom-mode)        ;; inicia o "writeroom"
(evil-ex-define-cmd "SessionCd" 'kass/session-cd)  ;; modo de "sessão"

(evil-define-minor-mode-key 'normal 'writeroom-mode (kbd "<up>") 'evil-previous-visual-line)
(evil-define-minor-mode-key 'normal 'writeroom-mode (kbd "<down>") 'evil-next-visual-line)

;; Trocar de janela com as setas do mouse
(evil-define-key 'normal 'global (kbd "C-w <up>") 'evil-window-up)
(evil-define-key 'normal 'global (kbd "C-w <down>") 'evil-window-down)
(evil-define-key 'normal 'global (kbd "C-w <left>") 'evil-window-left)
(evil-define-key 'normal 'global (kbd "C-w <right>") 'evil-window-right)

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

;; Funções personalizadas
(defun kass/session-cd (folder)
  (interactive "DPasta para a sessão: ")
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
(kass/load-custom-file "writeroom-mode.el")    ;; Modo writeroom para escrita
(kass/load-custom-file "ifind-mode.el")        ;; Encontra arquivos pelo nome

(defun ifind ()
  (interactive)
  (setq default-directory (kass/session-dir-or-default))
  (ifind-mode))

(setq
 gc-cons-threshold 16777216 ;; Oposto das primeiras linhas
 gc-cons-percentage 0.1)
