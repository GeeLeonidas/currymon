# Currymon
## Problemas conhecidos
- No momento em que isto é escrito, *builds* no Windows estão falhando devido a um [bug](https://github.com/haskell-game/sdl2/issues/277) no *wrapper* do SDL2 para Haskell.
- O *wrapper* do SDL2 não consegue detectar suporte à aceleração por *hardware* em algumas máquinas. Utilizou-se renderização por *software* como contramedida.

## Como reproduzir o ambiente utilizado
- Instale o [Stack](https://docs.haskellstack.org/en/stable/#how-to-install-stack)
- Instale as bibliotecas SDL2 com uma das opções:
  - (Recomendado!) [Gerenciador de pacotes Nix](https://nixos.org/download)
    - Utilize a integração do Stack com o Nix (adicione `--nix` ao final de um comando `stack`)
  - Sistemas baseados em Debian/Ubuntu
    - Execute `sudo apt-get install pkg-config libsdl2-dev libsdl2-image-dev libsdl2-ttf-dev`

## Instruções
- Executa-se o programa com `stack run`
- Comandos do jogo:
  - Confirmar - Tecla Z
  - Voltar - Tecla X
  - Navegação - Setas Direcionais
