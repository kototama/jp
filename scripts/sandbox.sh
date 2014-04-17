sandbox() {
  if [[ $1 == "exec" ]]
  then
    shift
    local dir=$PWD conf db
    while :; do
      conf=$dir/cabal.sandbox.config
      [[ ! -f $conf ]] || break
      if [[ -z $dir ]]; then
          echo "Cannot find cabal.sandbox.config" >&2
          return 1
      fi
      dir=${dir%/*}
    done

    db=$(sed -ne '/^package-db: */{s///p;q;}' "$conf")
    if [[ -d $db ]]; then
      pkg_path=$(command cabal sandbox hc-pkg list 2> /dev/null | grep \: | tac | sed 's/://' | paste -d: - -)
      if [[ $# == 0 ]]; then
        GHC_PACKAGE_PATH=${pkg_path} PATH=$(dirname $db)/bin:$PATH $SHELL
      else
        GHC_PACKAGE_PATH=${pkg_path} PATH=$(dirname $db)/bin:$PATH "$@"
      fi
    fi
  else
    command cabal $@
  fi
}
