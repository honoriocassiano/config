# Arquivos de configuração


No Windows, criar um arquivo `cfg.bat` com o seguinte conteúdo

```
git --git-dir="%HOME%\.dotfiles" --work-tree="%HOME%" %*
```

Adicionar a pasta que contém o arquivo ao `Path` do sistema operacional.

Caso esteja no Linux, adicionar a sequinte linha ao `.bashrc`

```
alias cfg="git --git-dir=$HOME/.dotfiles --work-tree=$HOME"
```

A partir de agora é possível utilizar o comando `cfg` para operar sobre os arquivos de configuração.

Para clonar o repositório, executar

```
cfg clone --bare git@github.com:honoriocassiano/config.git
```

Para suprimir a exibição dos arquivos não rastreados pelo git, utilizar

```
cfg config --local status.showUntrackedFiles no
```

