title_en="Seeds of the New World"
title_es="Semillas del Nuevo Mundo"
title_cz="Seminka Noveho Sveta"

run() { echo "$ $@" && $@ }

validate_date() {
  emulate -L zsh  # localises all option changes
  setopt extended_glob

  if [[ $1 == (#b)[0-9]##-[0-9]##-[0-9]## ]]; then
    return 0
  else
    return 1
  fi
}

save-function-list

load ~/.zsh/environments/basic.zsh

alias emacs="emacs --load $PWD/.env/emacs.el"
alias et="bb -cp src -m jakub-stastny.et.runner"

# build en
build() {
  local lang=$1 && shift
  local title

  case "$lang" in
    en) title=$title_en ;;
    es) title=$title_es ;;
    cz) title=$title_cz ;;
    *) echo "Unsupported language code: $lang"; return 1 ;;
  esac

  ./bin/convert.clj > output/content.$lang.tex && (cd output && run context $@ $title.tex)
}

# release en 2025-04-30
# release es 2025-04-30 2025-05-12
release() {
  if validate_date $2; then
    if [[ $1 == "en" ]]; then
      build $1 --release=$2
    else
      if validate_date $3; then
        build $1 --release=$2 --translation=$3
      else
        echo "Invalid translation date format"
      fi
    fi
  else
    echo "Invalid release date format"
  fi
}

report-custom-functions
