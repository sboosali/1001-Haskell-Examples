##################################################
# Makefile Settings
##################################################

SHELL=bash

.EXPORT_ALL_VARIABLES:

##################################################
# Makefile Variables: overrideable (via EnvironmentVariables)
##################################################

BaseDirectory ?=$(CURDIR)

NixDirectory       ?=./nix
Cabal2nixDirectory ?=$(NixDirectory)/packages/cabal2nix

#------------------------------------------------#

CabalTargets ?=all
CabalTarget  ?=lib:spiros

ProjectFile ?=./cabal.project

#------------------------------------------------#

LibraryTarget    ?=lib:spiros

ExecutableTarget ?=exe:example-spiros

#------------------------------------------------#

NixFile    ?=./nix/default.nix
NixTarget  ?=cabalProjects

#NixTargets ?=
#NixTarget  ?=packages.spiros

#------------------------------------------------#

Cabal      ?=cabal
CabalBuild ?=cabal new-build

CabalStatic ?=--enable-executable-static

#------------------------------------------------#

Nix      ?=nix
NixBuild ?=nix-build --show-trace
NixShell ?=nix-shell --show-trace

Cabal2nix ?=cabal2nix

#------------------------------------------------#

Open		?=xdg-open
Pandoc		?=pandoc
Markdown	?=multimarkdown

#------------------------------------------------#

CheckStaticExecutables ?=ldd

CheckCabal	?=$(Cabal) check
CheckTarball	?=tar -C /tmp -zxvf
CheckMarkdown	?=$(Markdown)
CheckJson	?=jsonlint
CheckBash	?=shellcheck
CheckNix	?=nix-instantiate

#------------------------------------------------#

RootDirectory ?=$(CURDIR)
DefaultPackageDirectory ?=$(DefaultPackageName)

#------------------------------------------------#

ReleaseDirectory ?=./ignore/release
#                          ^ [Customize]

# ReleaseDirectory ?=./release
# ^ change `ReleaseDirectory` to `./release` during a release
# to actually commit it.

BuildDirectory ?=./dist-newstyle
BuildTarballDirectory ?=$(BuildDirectory)/sdist/

NixDirectory ?=./nix
ScriptDirectory ?=./scripts
DocumentDirectory ?=./docs

HaddockDirectory ?=$(ReleaseDirectory)/documentation
TarballDirectory ?=$(ReleaseDirectory)/tarballs
BinaryDirectory ?=$(ReleaseDirectory)/bin
InstallDirectory ?=$(ReleaseDirectory)/dist-newstyle/ #TODO

#------------------------------------------------#



#------------------------------------------------#

##################################################
# Makefile Variables: not overrideable
##################################################

CabalOptions=--project-file $(ProjectFile) --builddir $(BuildDirectory)

##################################################
# the `default` target
##################################################

default: build

.PHONY: default

##################################################
# `nix` wrapper targets
##################################################

#------------------------------------------------#

nix-cabal-build:

	@echo -e "\n========================================\n"

	$(NixShell) shell.nix --run 'cabal new-build exe:calc --'

	@echo -e "\n========================================\n"

	$(CheckStaticExecutables) `$(NixShell) shell.nix --run 'cabal new-exec -- which calc'`

	@echo -e "\n========================================\n"

.PHONY: nix-cabal-build

#------------------------------------------------#

nix-cabal-run: nix-cabal-build

	@echo -e "\n========================================\n"

	$(NixShell) shell.nix --run 'cabal new-run exe:calc --' &disown

	@echo -e "\n========================================\n"

.PHONY: nix-cabal-run

#------------------------------------------------#

##################################################
# « cabal2nix »...
##################################################

#------------------------------------------------#

shell.nix: calc.cabal

	@echo "========================================"

	@echo "#  -*- mode: nix; buffer-read-only: t; -*-  " > "./shell.nix"

	$(Cabal2nix) --shell "file://./." >> "./shell.nix"

	@echo "========================================"

	@cat "./shell.nix"

# .PHONY:

#------------------------------------------------#

calc.nix: calc.cabal

	@echo "========================================"

	mkdir -p "$(Cabal2nixDirectory)"

	@echo "#  -*- mode: nix; buffer-read-only: t; -*-  " > "$(Cabal2nixDirectory)/calc.nix"

	(cd "$(Cabal2nixDirectory)"  &&  $(Cabal2nix) "-fstatic" "file://$(BaseDirectory)/." >> "./calc.nix")

        # ^ Nix PathLiterals are relative to their source file (not the directory of an invoked command).

	@echo "========================================"

	@cat "$(Cabal2nixDirectory)/calc.nix"

.PHONY: calc.nix

#------------------------------------------------#

cabal2nix-static:

	@echo "========================================"

	mkdir -p "$(Cabal2nixDirectory)"

	@echo "#  -*- mode: nix; buffer-read-only: t; -*-  " > "$(Cabal2nixDirectory)/spiros.nix"

	(cd "$(Cabal2nixDirectory)"  &&  $(Cabal2nix) "-fstatic" "file://$(BaseDirectory)/spiros" >> "./spiros.nix")

        # ^ Nix PathLiterals are relative to their source file (not the directory of an invoked command).

	@echo "========================================"

	@cat "$(Cabal2nixDirectory)/spiros.nix"

.PHONY: cabal2nix-static

#------------------------------------------------#

cabal2nix-dynamic:

	@echo "========================================"

	mkdir -p "$(Cabal2nixDirectory)"

	@echo "#  -*- mode: nix; buffer-read-only: t; -*-  " > "$(Cabal2nixDirectory)/spiros.nix"

	(cd "$(Cabal2nixDirectory)"  &&  $(Cabal2nix) "file://$(BaseDirectory)/spiros" >> "./spiros.nix")

        # ^ Nix PathLiterals are relative to their source file (not the directory of an invoked command).

	@echo "========================================"

	@cat "$(Cabal2nixDirectory)/spiros.nix"

.PHONY: cabal2nix-dynamic

#------------------------------------------------#

##################################################
# Executables ####################################
##################################################

#------------------------------------------------#

example:

	$(CabalBuild) $(CabalOptions) "exe:example-spiros"

	$(Cabal) new-exec $(CabalOptions) -- example-spiros --version

	$(Cabal) new-install $(CabalOptions) --overwrite-policy=always "exe:example-spiros"

	ldd `which example-spiros`

#	cabal new-exec --project-file ./cabal.project -- ldd `which example-spiros`

.PHONY: example

#------------------------------------------------#

##################################################
# Release ########################################
##################################################

#------------------------------------------------#

sdist:

	$(Cabal) new-build $(CabalTargets)
	$(Cabal) new-sdist $(CabalTargets)

.PHONY: sdist

#------------------------------------------------#

static: cabal-static.project

	$(Cabal) new-build -fstatic $(CabalStatic) --project-file="./cabal-static.project" exe:example-spiros

# --enable-executable-static
# -fstatic
# --project-file="./cabal-static.project"
# --extra-lib-dirs="/usr/lib/x86_64-linux-gnu"
# --extra-lib-dirs="/nix/store/blfgah5rv7h3qzl2gv6p6d8i2sxh0vgl-musl-1.1.21/lib /nix/store/pdyjwbhb77k17n6gl78a87a70gywr8dk-gmp-6.1.2/lib /nix/store/vz8iz7ws35aww6i8521z4964xp5akalh-libffi-3.2.1/lib"

.PHONY: static

#------------------------------------------------#

##################################################