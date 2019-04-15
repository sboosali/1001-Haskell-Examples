##################################################
# Makefile Directives
##################################################

.EXPORT_ALL_VARIABLES:

##################################################
# Makefile Variables: Package
################################################## Customize:
# (i.e. Package-Specific / Component-Specific)

DefaultPackageName=egg-haskell
#                          ^ [Customize]

DefaultPackageVersion=0.0.0
#                          ^ [Customize]

DefaultModule=Egg
#                          ^ [Customize]

DefaultLibraryTarget="lib:$(DefaultPackageName)"
#                          ^ [Customize]

DefaultPackage=$(DefaultPackageName)-$(DefaultPackageVersion)
#                          ^ [Derived]

DefaultTarget="all"
#                          ^ [Customize]

##################################################
# Makefile Variables: Haskell Compiler.
################################################## Customize:

CompilerFlavor=ghc
#                          ^ [Customize]

CompilerVersion=8.4.3
#                          ^ [Customize]

CompilerProgram=$(CompilerFlavor)-$(CompilerVersion)
#                          ^ [Derived]

##################################################
# Makefile Variables: Project / `cabal-new`
##################################################

ProjectFile=./cabal.project
#                          ^ [Customize]

CabalOptions=--project-file $(ProjectFile) -w $(CompilerProgram)
#                          ^ [Derived]

##################################################
# Makefile Variables: Programs.
##################################################

Cabal=cabal

Markdown=multimarkdown
 #TODO pandoc

Open=xdg-open

CheckCabal=$(Cabal) check
CheckTarball=tar -C /tmp -zxvf
CheckMarkdown=$(Markdown)
CheckJson=jsonlint
CheckBash=shellcheck
CheckNix=nix-instantiate
 # ^ nix-instantiate:
 # parse the given `.nix`, and return its `.drv` file.

##################################################
# Makefile Variables: File/Directory Paths
##################################################

ReleaseDirectory=./ignore/release
#                          ^ [Customize]

# ReleaseDirectory=./release
# ^ change `ReleaseDirectory` to `./release` during a release
# to actually commit it.

RootDirectory=$(CURDIR)
DefaultPackageDirectory=$(DefaultPackageName)

BuildDirectory=./dist-newstyle
NixDirectory=./nix
ScriptDirectory=./scripts
DocumentDirectory=./docs

HaddockDirectory=$(ReleaseDirectory)/documentation
TarballDirectory=$(ReleaseDirectory)/tarballs

##################################################
# Makefile Targets: Standard #####################
##################################################

all: file-embded

.PHONY: all

##################################################
# Makefile Targets: Haskell ######################
##################################################

file-embded:

	$(Cabal) new-build "./pattern-file-embded/egg-file-embed.hs"

.PHONY: file-embded

#------------------------------------------------#

run-file-embded:

	$(Cabal) new-run "./pattern-file-embded/egg-file-embed.hs"

.PHONY: run-file-embded

##################################################
# Makefile Targets: Custom #######################
##################################################

build-examples:

	$(Cabal) new-build xmlrpc-examples

.PHONY: build-examples

##################################################

examples: build-examples
	@echo '=================================================='
	$(Cabal) new-run xmlrpc-example-time
	@echo '=================================================='
	$(Cabal) new-run xmlrpc-example-validator
	@echo '=================================================='
	$(Cabal) new-run xmlrpc-example-introspect
	@echo '=================================================='
	$(Cabal) new-run xmlrpc-example-simple
	@echo '=================================================='
	$(Cabal) new-run xmlrpc-example-person
	@echo '=================================================='

.PHONY: examples

##################################################