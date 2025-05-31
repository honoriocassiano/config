(setq
 gc-cons-threshold 402653184 ; Teoricamente aumenta a velocidade de inicialização
 gc-cons-percentage 0.6)

(setq custom-file "~/.emacs.d/custom.el") ; Arquivo diferenciado para salvar as variáveis customizadas

(ido-mode 1)         ; Autocomplete
(ido-everywhere 1)   ; Autocomplete (de novo)
(menu-bar-mode -1)   ; Remove a barra de menu
(tool-bar-mode -1)   ; Remove a barra de ferramentas
(scroll-bar-mode -1) ; Remove a barra de rolagem
(column-number-mode 1) ; Exibe (linha, coluna) na barra de status
(blink-cursor-mode -1) ; Cursos para de piscar
(tooltip-mode -1)      ; Não lembro
(recentf-mode 1)       ; Não sei

(setq-default
 indent-tabs-mode nil) ; Alguma coisa relativa a tabs vs espaços

(setq
 display-line-numbers-type 'relative ; Números relativos nas linhas
 inhibit-splash-screen 1       ; Sem tela inicial
 inhibit-startup-screen 1      ; (Quase) a mesma merda
 scroll-conservatively 10000   ; Para de pular o cursor quando rola para fora do "campo de visão"
 scroll-margin 3               ; Borda para rolagem vertical
 initial-scratch-message nil   ; Não exibe uma mensagem inicial no buffer *scratch*
 recentf-max-saved-items nil   ; Não sei
 tab-width 4                   ; Tab são 4 espaços
 c-basic-offset 4              ; Mesma merda só que diferente
 make-backup-files nil         ; Não salva arquivos de backup
 scroll-step 1                 ; Scroll do mouse funcional
 display-line-numbers-width 4  ; Quantidade de dígitos reservados para os números das linhas
 split-height-threshold 20     ; Tamanho máximo para uma nova "janela"
 compilation-window-height 15) ; Tamanho da "janela" de compilação

(add-to-list 'default-frame-alist '(fullscreen . maximized)) ; Maximiza a janela após a inicialização

(global-display-line-numbers-mode 1) ; Exibe as linhas
(global-hl-line-mode 1)              ; Destaque na linha atual
(global-auto-revert-mode 1)          ; Não pede confirmação para recarregar um arquivo do disco

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

(set-frame-font "-UKWN-Iosevka Custom-regular-normal-expanded-*-15-*-*-*-m-0-iso10646-1") ; Modo horroroso de especificar a fonte

(setq split-width-threshold nil) ; Não lembro

(global-set-key (kbd "<f9>") 'compile) ; Compila
(global-set-key (kbd "<f6>") 'dired)   ; Abre um diretório

; C-z é esquisito então eu desabilito no modo evil
(define-key evil-insert-state-map (kbd "C-z") nil)
(define-key evil-motion-state-map (kbd "C-z") nil)
(define-key evil-visual-state-map (kbd "C-z") nil)
(define-key evil-replace-state-map (kbd "C-z") nil)
(define-key evil-operator-state-map (kbd "C-z") nil)
; E no emacs
(global-set-key (kbd "C-z") nil)

(evil-ex-define-cmd "W" 'evil-write)     ; w ou W salvam
(evil-ex-define-cmd "E" 'evil-edit)      ; e ou E salvam

 ; Comandos personalizados do Evil
(evil-ex-define-cmd "Goyo" 'writeroom-mode)        ; inicia o "writeroom"
(evil-ex-define-cmd "SessionCd" 'kass/session-cd)  ; modo de "sessão"

; Trocar de janela com as setas do mouse
(evil-define-key 'normal 'global (kbd "C-w <up>") 'evil-window-up)
(evil-define-key 'normal 'global (kbd "C-w <down>") 'evil-window-down)
(evil-define-key 'normal 'global (kbd "C-w <left>") 'evil-window-left)
(evil-define-key 'normal 'global (kbd "C-w <right>") 'evil-window-right)

; Funções personalizadas
(defun kass/session-cd (folder)
  (interactive "DPasta para a sessão: ")
  (setq default-directory folder)
  (setq-default kass/session-directory folder))

(add-hook 'find-file-hook
          (lambda ()
            (if (boundp 'kass/session-directory)
                (setq default-directory kass/session-directory)
              nil)))

(load-file custom-file) ; Carrega o arquivo com as variáveis customizadas

(setq
 gc-cons-threshold 16777216 ; Oposto das primeiras linhas
 gc-cons-percentage 0.1)
