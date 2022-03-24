PATH="$PATH:$PWD/cabal-dev/bin/"

VERSION=$(awk '/^version:/{print $2}' knob.cabal)

CABAL_DEV=$(which cabal-dev)
XZ=$(which xz)

require_cabal_dev()
{
	if [ -z "$CABAL_DEV" ]; then
		echo "Can't find 'cabal-dev' executable; make sure it exists on your "'$PATH'
		echo "Cowardly refusing to fuck with the global package database"
		exit 1
	fi
}

clean_dev_install()
{
	require_cabal_dev
	
	rm -rf dist
	$CABAL_DEV install || exit 1
}
