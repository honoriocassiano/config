# Arquivos de configura��o


No Windows, criar um arquivo `cfg.bat` com o seguinte conte�do

```
git --git-dir="%HOME%\.dotfiles" --work-tree="%HOME%" %*
```

Adicionar a pasta que cont�m o arquivo ao `Path` do sistema operacional.

Caso esteja no Linux, adicionar a sequinte linha ao `.bashrc`

```
alias cfg="git --git-dir=$HOME/.dotfiles --work-tree=$HOME"
```

A partir de agora � poss�vel utilizar o comando `cfg` para operar sobre os arquivos de configura��o.

Para clonar o reposit�rio, executar

```
cfg clone --bare git@github.com:honoriocassiano/config.git
```

Para suprimir a exibi��o dos arquivos n�o rastreados pelo git, utilizar

```
cfg config --local status.showUntrackedFiles no
```

